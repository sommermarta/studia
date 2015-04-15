library(shiny)
library(dplyr)
library(ggplot2)

load("C:\\Users\\sommerm\\Dropbox\\TWDwR\\PISAeurope.Rda")

pol <- pisa %>% filter(CNT == 'Poland')
shinyServer(function(input, output) {
  output$smoothPlot <- renderPlot({
    ggplot(pol, aes(x=ESCS, y=PV1MATH)) +
      geom_point(alpha=0.5) +
      geom_smooth(colour="blue", size=2,
                  span=input$smooth, method="loess")
  })
})

