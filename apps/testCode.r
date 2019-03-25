# runApp('testCode.r') from the R terminal 

library(shiny)
## User interface with slider (input) and plot (output)
ui <- fluidPage(
    sliderInput("n", "Number of samples", 1, 100, 10),
    plotOutput("hist"),
    radioButtons("dist", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "unif",
                 "Exponential" = "exp")),
    plotOutput("distPlot")
)
## Server function connecting input and output
server <- function(input, output){
    output$hist <- renderPlot({
        if(input$dist == "norm"){
            x <- rnorm(input$n)
            # draw n random values
            hist(x)
            }
        else if (input$dist == "unif"){
            x <- runif(input$n)
            hist(x)
            }   
        else if (input$dist == "exp"){
            x <- rexp(input$n)
            hist(x)
            }
        })
    }
shinyApp(ui = ui, server = server)
