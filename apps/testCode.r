# runApp('testCode.r') from the R terminal 

library(shiny)
## User interface with slider (input) and plot (output)
ui <- fluidPage(
    sliderInput("n", "Number of samples", 1, 100, 10),
    plotOutput("hist")
)
## Server function connecting input and output
server <- function(input, output){
    output$hist <- renderPlot({
        x <- rnorm(input$n)
        # draw n random values
        hist(x)
        })
    }
shinyApp(ui = ui, server = server)
