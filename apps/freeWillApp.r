library(shiny)
library(shinythemes)
library(caret)
library(kernlab)
library(e1071)

svm<- function(x){
  x.lag <- c(NA, x[1:length(x)-1])
  x.test <- as.numeric(tail(x,round(length(x)*0.1, 0)))
  x.test.lag <- as.numeric(tail(x.lag, round(length(x)*0.1, 0)))
  x.train <- as.numeric(head(x,length(x)-round(length(x)*0.1, 0)))
  x.train.lag <- as.numeric(head(x.lag,length(x.lag)-round(length(x)*0.1, 0)))

  df.train <- na.omit(data.frame(as.factor(x.train), as.factor(x.train.lag)))
  df.test <- na.omit(data.frame(as.factor(x.test), as.factor(x.test.lag)))

  colnames(df.train) <- c("x", "xlag")
  colnames(df.test) <- c("x", "xlag")

  trctrl <- trainControl(method = "repeatedcv")

  svm_linear <- train(x~xlag, data = df.train, method = "svmLinear", trControl = trctrl)
  test_pred <- predict(svm_linear, newdata = df.test)

  a <- confusionMatrix(table(test_pred, newdata = df.test$x))

  grid <- expand.grid(C = c(0.1, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.50, 1.75, 2.5))
  svm_Linear_Grid <- train(x~xlag, data = df.train, method = "svmLinear", trControl = trctrl, tuneGrid = grid, tuneLength = 10)
  svm_Linear_Grid
  plot(svm_Linear_Grid)

  test_pred_grid <- predict(svm_Linear_Grid, newdata = df.test)
  test_pred_grid
  confusionMatrix(table(test_pred_grid, df.test$x))
  a$overall[1]
}
svm(x)
data<-c(data, rbinom(10, 1, .55))
x<-data[1:21]

runApp( list(ui = fluidPage(theme = shinytheme("darkly"),
                            titlePanel("Is free will an illusion?"),
                            hr(),
                            sidebarPanel(h3("Challenge"),
                                         p("Try to", strong("beat"), "the model by being", strong("unpredictable.")),
                                         h4("How it works"),
                                         p("Press left and right in such a", strong("random"), "way that the model is unable to predict your response."),
                                         p("Try to do this", strong("many"), "times. When you are done, press the button below to see how well you and the model have done."),
                                         p("The model wins when it can predict you with an accuracy of 0.6 or higher. You can keep collecting data and run the analysis again."),
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
                                                       p("Here, you will see the accuracy of the model."),
                                                       textOutput("model")

                                             )
                                   )
                            )
)

, server = function(input, output, session) {

  output$results = renderText({
    if(length(input$mydata[1]) == 0){
      "Press 'F' for left and 'J' for right."
    }
    else if(input$mydata[1] == 70){
      "You pressed left."
    }
    else if(input$mydata[1] == 74){
      "You pressed right."
    }

  })

  outputdata<-reactive({
    input$mydata[1]
  })

  runModel <- reactive({
    if(input$run != 0){
      data <- read.table(colorfile)
      data[which(data==74)] <- 1
      data[which(data==70)] <- 0
      data <- as.numeric(data)
      model <- svm(data)
      return(round(model, 3))
      input$run<-0
    }
  })

  output$model <- renderText({
    if(input$run != 0){
      if(runModel() < 0.6){
        c("You won! The model could not predict you. It had an accuracy of", runModel(), ".")
      }
      else c("You lost.. :( The model predicted your strokes with an accuracy of", runModel(), ".")
    }
  })

  observe({
    close( file( "colorfile.txt", open="w" ) )
  })

  observe({
    message <- paste(outputdata()," ")
    cat(message,file=colorfile, append=TRUE)
  })

}
)
)

