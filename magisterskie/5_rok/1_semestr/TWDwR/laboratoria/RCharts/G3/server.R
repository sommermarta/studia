## server.r

require("shiny")
require("rCharts")
library("dplyr")
library("ggplot2")

setwd("C:\\Users\\sommerm\\Dropbox\\TWDwR\\RCharts\\G3")

# shiny:

shinyServer(function(input, output, session) {
  output$dekompozycja <- renderChart({
    
    # ciagla:
    
    kobiety <- read.table("fltper_1x1.txt", skip=2, header=T)
    mezczyzni <- read.table("mltper_1x1.txt", skip=2, header=T)
    
    kobiety <- cbind(kobiety,plec=rep('K',nrow(kobiety)))
    mezczyzni <- cbind(mezczyzni,plec=rep('M',nrow(mezczyzni)))
    
    razem <- rbind(mezczyzni,kobiety)
    
    razem2005logciagla <- razem %>%
      filter(Year==as.numeric(input$rok)) %>%
      mutate(Age = as.numeric(as.character(Age))) %>%
      mutate(qx = log(qx,10)) %>%
      mutate(qx = round(qx-min(qx),2)) %>%
      group_by(plec)
    
    # dyskretna:
    
    kobiety <- read.table("fltper_5x1.txt", skip=2, header=T)
    mezczyzni <- read.table("mltper_5x1.txt", skip=2, header=T)
    
    kobiety <- cbind(kobiety,plec=rep('K',nrow(kobiety)))
    mezczyzni <- cbind(mezczyzni,plec=rep('M',nrow(mezczyzni)))
    
    razem <- rbind(mezczyzni,kobiety)
    
    razem2005logdyskretna <- razem %>%
      filter(Year == as.numeric(input$rok)) %>%
      mutate(qx = log(qx,10)+3.4) %>%
      group_by(plec)
    
    p1 <- nPlot(qx ~ Age, group = "plec", data = razem2005logciagla[-c(111,222),], 
                type = "lineWithFocusChart")
   
    p1$addParams(dom = 'dekompozycja')
    #p1$set(width=input$szerokosc)
    
    p2 <- nPlot(qx ~ Age, group = "plec", data = razem2005logdyskretna, 
                type = "multiBarChart")
    p2$addParams(dom = 'dekompozycja')
    
    
    if(input$rodzaj=="slupkowy") return(p2) else return(p1)
  })
})

# shiny::runApp('C:\\Users\\sommerm\\Dropbox\\TWDwR\\RCharts\\G3')
# install.packages("devtools")
# install_github('rCharts', 'ramnathv')


