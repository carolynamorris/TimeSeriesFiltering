library(shiny)

shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("family",
                   "Family:",
                   c("Symmetric" = "symm",
                     "Gaussian" = "gauss"),
                   inline = T
      ),
      br(),
      sliderInput("span",
                  "Span:",
                  min = 0,
                  max = 0.05,
                  value = 0.00414)
    ),
    
    mainPanel(
      textOutput("RMSE"),
      plotOutput("timeSeries")
    )
  )
))