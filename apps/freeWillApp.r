library(shiny)
library(shinythemes)

colorfile <- 
# paste(getwd(),"colorfile.txt",open="w", sep="/")

runApp( list(ui = fluidPage(theme = shinytheme("darkly"),
                            titlePanel("Is free will an illusion?"),
                            hr(),
                            sidebarPanel(h3("Challenge"), 
                                         p("Try to", strong("beat"), "the mode by being", strong("unpredictable.")),
                                         h4("How it works"),
                                         p("Press left and right in such a", strong("random"), "way that the model is unable to predict your response."),
                                         p("Try to do this", strong("many"), "times. When you are done, press the button below to see how well you and the model have done."),
                                         p("Press", strong("'F'"), "for left and", strong("'J'"), "for right."),
                                         actionButton("run", label = "See results")),
                            column(8, 
                                   mainPanel(verbatimTextOutput("results"),
                                             tags$script('
                                                         $(document).on("keydown", function (e) {
                                                         if(e.keyCode == 70 | e.keyCode == 74){
                                                         Shiny.onInputChange("mydata", [e.which, e.timeStamp])};
                                                         });
                                                         '),
                                             wellPanel(h4("Model Predictions"),
                                                       p("Here, it will be shown how many times the model predicted your response."),
                                                       textOutput("model"))
                                             
                                             )
)
)

, server = function(input, output, session) {
  
  output$results = renderText({
    if(length(input$mydata) == 0){
      "Press 'F' for left and 'J' for right."
    }
    else if(input$mydata == 70){
      "You pressed left."
    }
    else if(input$mydata == 74){
      "You pressed right."
    }
    else{
        "you are goat"
    }
  })
  
  outputdata<-reactive({
    input$mydata[1]
  })
  
  runModel <- reactive({ 
    if(input$action == 0){
    data <- read.table(colorfile)
    data[which(data==74)] <- 1
    data[which(data==70)] <- 0
    
    model <- glm(data[1:10] ~ data[11:20])

  }
    })
  
  output$model <- renderPrint({
    summary(runModel(), data = intput$action)
  })

  
  observe({
    message <- paste(outputdata()," ")
    cat(message,file=colorfile, append=TRUE)
  })
  
}
)
)

