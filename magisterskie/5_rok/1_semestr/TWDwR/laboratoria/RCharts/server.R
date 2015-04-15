## server.r

require("shiny")
require("rCharts")

setwd("C:\\Users\\sommerm\\Dropbox\\TWDwR\\RCharts")
load("mmG.rda")

shinyServer(function(input, output, session) {
  output$dekompozycja <- renderChart({
    
    p1 <- nPlot(value ~ daty, group = "variable", data = mmG, type = "multiBarChart")
   
    p1$chart(stacked = TRUE)
    p1$addParams(dom = 'dekompozycja')
    p1$set(width=input$szerokosc)
    
    return(p1)
  })
})

# shiny::runApp('C:\\Users\\sommerm\\Dropbox\\TWDwR\\RCharts')
# install.packages("devtools")
# install_github('rCharts', 'ramnathv')