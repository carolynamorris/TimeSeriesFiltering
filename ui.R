library(shiny)

shinyUI(fluidPage(
  titlePanel("Filters for Outlier Detection"),
  fluidRow(
   
    column(4, 
      wellPanel(
        h4("Filter"),
        radioButtons("filter",
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
    ))
  ),
  
  fluidRow(
    column(12,
      wellPanel(
        plotOutput("timeSeries"),
        plotOutput("residuals")
      )       
    )  
  )
))
