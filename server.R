library(shiny)
library(jsonlite)
library(zoo)

URL <- "~/Copy/CitiLogics/Public/dev/loess-app/mason.tank.may.2013.json"
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
ts <- zoo(value, time)
outliers <- c(272, 299, 360, 464, 806, 1011, 1182, 1560, 2080, 2511, 2538, 2608)

# Define server logic required to draw a line chart
shinyServer(function(input, output) {
  
  output$param1 <- renderUI({
    if (is.null(input$model))
      return()
    
    switch(input$model,
      "loess" = radioButtons("family",
                             "Family:",
                             c("Symmetric" = "symm",
                              "Gaussian" = "gauss"),
                             inline = T
                              ),
      "movingmedian" = numericInput("windowmm",
                                    "Window Size:",
                                    value = 5,
                                    min = 3,
                                    max = 100),
      "movingaverage" = numericInput("windowma",
                              "Window Size:",
                              value = 5,
                              min = 3,
                              max = 100),
    )
  })
  
  output$param2 <- renderUI({
    if (is.null(input$model))
      return()
    
    switch(input$model,
      "loess" = sliderInput("span",
                            "Span:",
                            min = 0,
                            max = 0.05,
                            value = 0.00634),
      "movingmedian" = return(),
      "movingaverage" = return() 
    )
  })
  
  output$RMSE <- renderPrint({ 
    if (input$model == "loess") {  
      l <- loess(value~time, data=df, span=input$span, degree=2, family=input$family)
      rmse.loess <- round(sqrt(mean(l$residuals^2)), digits = 6)
      rmse.loess
    } else if (input$model == "movingmedian") {
      mm <- rollmedian(ts, input$windowmm)
      rmse.mm <- round(sqrt(mean((value - mm)^2)), digits = 6)
      rmse.mm
    } else if (input$model == "movingaverage") {
      ma <- rollmean(ts, input$windowma)
      rmse.ma <- round(sqrt(mean((value - ma)^2)), digits = 6)
      rmse.ma
    }
  })
  
  output$timeSeries <- renderPlot({
    plot(value~time, cex = 0.8, main="Tank Level vs. Time",
         ylab="Tank Level (feet)", xlab="Time",
         col=ifelse(index(value) %in% outliers, "red", "black"),
         pch=ifelse(index(value) %in% outliers, 19, 1))
    if (input$model == "loess") {
      l <- loess(value~time, data=df, span=input$span, degree=2, family=input$family)
      lines(l$fitted~time, col='dodgerblue3', lwd=2.7)      
    } else if (input$model == "movingmedian") {
      mm <- rollmedian(ts, input$windowmm)
      lines(mm, col='forestgreen', lwd=2.7)
    } else if (input$model == "movingaverage") {
      ma <- rollmean(ts, input$windowma)
      lines(ma, col="orange", lwd=2.7)
    }
  })
  
  output$residuals <- renderPlot({
    if (input$model == "loess") {
      l <- loess(value~time, data=df, span=input$span, degree=2, family=input$family)
      residuals <- l$residuals
      time2 <- time
    } else if (input$model == "movingmedian") {
      mm <- rollmedian(ts, input$windowmm, align="center")
      residuals <- coredata(mm) - coredata(ts[1:length(mm)])
      time2 <- time[1:length(mm)]
    } else if (input$model == "movingaverage") {
      ma <- rollmean(ts, input$windowma, align="center")
      residuals <- coredata(ma) - coredata(ts[1:length(ma)])
      time2 <- time[1:length(ma)]
    }
    
    plot(time2, residuals, main="Residuals vs. Time",
         ylab="Residual (feet)", xlab="Time",
         col=ifelse(index(value) %in% outliers, "red", "black" ),
         pch=ifelse(index(value) %in% outliers, 19, 1))
  })
})
