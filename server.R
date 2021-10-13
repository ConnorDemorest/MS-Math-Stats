library(shiny)
library(ggplot2)
library(gridExtra)

shinyServer(function(input, output) {
    output$distPlot <- renderPlot({
        # It's almost certainly bad practice to put everything in the "renderPlot" thing but it was being annoying
        # Does the function cover the theta value
        # Set seed to get the same data to resample from as we had when we did theoretical methods
        covers_theta = function(theta, lcl, ucl) {
            return(ifelse((lcl <= theta & theta <= ucl), 1, 0))
        }
        
        # What is the theoretical LCL and UCL for the data, assuming normality?
        theo_95ci = function(val, n, conf) {
            phat = val/n
            SE = sqrt(phat*(1-phat)/n)
            lcl = val/n + qnorm((1-conf)/2)*SE
            ucl = val/n + qnorm(1-(1-conf)/2)*SE
            return(c(lcl, ucl))
        }
        
        funct = function(num_repetitions, sample_size, confidence_level, p){
            # Simulate data from binomial model given the values specified by user
            dat = rbinom(n = num_repetitions, 
                         size = sample_size, 
                         prob = p)
            covers_theo = vector(length = num_repetitions)
            for (i in 1:num_repetitions) {
                ci = theo_95ci(val = as.numeric(dat[i]), n = sample_size, conf = as.double(confidence_level))
                covers_theo[i] = covers_theta(theta = p, lcl = ci[1], ucl = ci[2])
            }
            return(mean(covers_theo))
        }
        
        # bootstrap coverage of the value
        funct_boot = function(num_repetitions, sample_size, confidence_level, p) {
            covers_boot = vector(length = num_repetitions)
            dat = rbinom(n = num_repetitions, 
                         size = sample_size, 
                         prob = p)
            for (j in 1:num_repetitions){
                sim_pop = c(rep(1, dat[j]), rep(0, sample_size - dat[j]))
                sim_props <- rep(NA, 100)
                for (i in 1:100) {
                    sim_props[i] <- mean(sample(sim_pop, sample_size, replace = TRUE))
                }
                low_ci <- quantile(sim_props, (1 - as.double(confidence_level))/2)
                high_ci <- quantile(sim_props, 1 - (1 - as.double(confidence_level))/2)
                covers_boot[j] = covers_theta(theta = p, lcl = low_ci, ucl = high_ci)
            }
            return(mean(covers_boot))
        }
    # Make Plot!
    # Fixed sample size, variable values of p

    prob_vals = seq(from = 0.03, to = 0.97, length.out = 40)
    a1 = mapply(funct, 
                input$num_repetitions, 
                input$sample_size, 
                input$confidence_level,
                prob_vals)
    a2 = mapply(funct_boot, 
                input$num_repetitions, 
                input$sample_size, 
                input$confidence_level,
                prob_vals)

    df1 = data.frame(Covers = c(a1, a2), 
                     Type = rep(c("Theory", "Bootstrap"), each = length(prob_vals)),
                     N = rep(20, times = 2 * length(prob_vals)),
                     Prob = rep(prob_vals, times = 2))
    
    p1 = ggplot(data = df1) + 
        geom_smooth(aes(x = Prob, y = Covers, col = Type), method = "loess", formula = "y~x", se = F) +
        geom_hline(yintercept = 0.95, linetype = "dashed") +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        xlim(c(0, 1)) +
    labs(x = "Probability of success",
         y = paste0("Coverage Prob by ", as.numeric(input$confidence_level) * 100, "% CI"),
         col = "Type of CI",
         title = paste0("Comparison of bootstrap vs theoretical methods for fixed sample size (N = ", input$sample_size, ")"))

    # Fixed p, variable sample size
    samp_vals = floor(seq(5, 100, length.out = 30))
    a3 = mapply(funct, 
                input$num_repetitions, 
                samp_vals, 
                input$confidence_level,
                input$p)
    a4 = mapply(funct_boot, 
                input$num_repetitions, 
                samp_vals, 
                input$confidence_level,
                input$p)
    
    df2 = data.frame(Covers = c(a3, a4), 
                     Type = rep(c("Theory", "Bootstrap"), each = length(samp_vals)),
                     Prob = rep(input$p, times = 2 * length(samp_vals)),
                     N = rep(samp_vals, times = 2))
    
    p2 = ggplot(data = df2) + 
        geom_smooth(aes(x = N, y = Covers, col = Type), method = "loess", formula = "y~x", se = F) +
        geom_hline(yintercept = 0.95, linetype = "dashed") +
        theme_bw() + 
        ylim(0, 1) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Sample Size",
             y = paste0("Coverage Prob by ", as.numeric(input$confidence_level) * 100, "% CI"),
             col = "Type of CI",
             caption = "Expected coverage should be at 95% (black dashed line), less than that is problematic.", 
             title = paste0("Comparison of bootstrap vs theoretical methods for fixed probabilty of success (p = ", input$p, ")"))
    
    grid.arrange(p1, p2, ncol = 1)
    })
})
