library(shiny)

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Theoretical coverage properties for single proportion data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("Note: App will be much faster if the number of repetitions is smaller.
                     More repetitions would give more accurate results. Defaults to 500."),
            numericInput("num_repetitions",
                        "Number of repetitions:",
                        value = 500),
            sliderInput("sample_size",
                        "Size of each sample:",
                        min = 10,
                        max = 200,
                        value = 20),
            sliderInput("p",
                        "Probability of success:",
                        min = 0,
                        max = 1,
                        value = 0.5, 
                        round = TRUE, 
                        step = 0.01),
            # Why do radio buttons look weird
            radioButtons("confidence_level",
                         "Confidence Level",
                         choices = list("90%" = 0.9, 
                                        "95%" = 0.95,
                                        "99%" = 0.99,
                                        "99.9%" = 0.999),
                        selected = 0.95)
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
