library(shiny)
library(jsonlite)
library(zoo)
URL <- "/Users/carolyn/Desktop/mason.tank.may.2013.json"

tank <- fromJSON(URL) # list
mydata <- tank$series$data # list
d <- mydata[[1]] # matrix (2 columns)

time <- d[,1] # numeric
value <- d[,2] # numeric
# convert from timestamp to human readable
time <- as.POSIXct(time, origin="1970-01-01", tz="GMT")
#boxplot.stats(value, coef=0.75, do.out=T)
m <- cbind(value, time)
df <- as.data.frame(m) 


# Define server logic required to draw a line chart
shinyServer(function(input, output) {
  
  output$timeSeries <- renderPlot({
    plot(value~time, cex = 0.7)
    l <- loess(value~time, data=df, span=input$span, degree=2, family=input$family)
    lines(l$fitted~time, col='dodgerblue3', lwd=2.8)
  })

  output$RMSE <- renderText({
    l <- loess(value~time, data=df, span=input$span, degree=2, family=input$family)
    rmse <- round(sqrt(mean(l$residuals^2)), digits = 6)
    paste("RMSE: ", rmse)
  })
  
})
