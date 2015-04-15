## ui.R

library(shiny)
library(rCharts)

shinyUI(fluidPage( conditionalPanel(
 
  titlePanel("Przykład rCharts")),
 
  sidebarLayout(
    sidebarPanel(
      selectInput("rodzaj", label = "Wybierz rodzaj wykresu:",
                  choices = list("slupkowy", "liniowy"),
                  selected = "liniowy"),
      selectInput("rok", label = "Wybierz rok:",
                  choices = as.character(1958:2009),
                  selected = "2009")
    ),
    
    mainPanel(
      h3(p("G3 - Jak długość życia [jej rozkład] różni się pomiędzy płciami?")),
      h3(p(" ")),
      showOutput("dekompozycja", lib="NVD3"),  # biblioteka javascriptowa
      plotOutput("smoothPlot", width=1200, height=800)
    )
  ))
)

