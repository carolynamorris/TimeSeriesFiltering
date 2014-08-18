library(shiny)

shinyUI(fluidPage(
  titlePanel("Models for Outlier Detection"),
  fluidRow(
   
    column(4, 
      wellPanel(
        h4("Model"),
        radioButtons("model",
                      "",
                      c("LOESS" = "loess",
                      "Moving Median" = "movingmedian",
                      "Moving Average" = "movingaverage"),
                      inline = T)
    )),
    
    column(4, 
      wellPanel(
        h4("Parameters"),
        
        # this outputs the dynamic UI component
        uiOutput("param1"),
        uiOutput("param2")
    )),
    
    column(4, 
      wellPanel(
        h4("RMSE"),
        verbatimTextOutput("RMSE")
    )),
    
    mainPanel(
      plotOutput("timeSeries"),
      plotOutput("residuals")
    )
  )
))
