## ui.R

library(shiny)
library(rCharts)

shinyUI(fluidPage( conditionalPanel(
 
  titlePanel("Przykład rCharts")),
 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Szerokość wykresu", inputId = 'szerokosc', 
                  min = 200, max = 1000, value=600)
    ),
    
    mainPanel(
      showOutput("dekompozycja", lib="NVD3")  # biblioteka javascriptowa
    )
  ))
)