library(shiny)

shinyUI(fluidPage(
  titlePanel("Moja pierwsza aplikacja"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("smooth",
                  "Wsp. przezroczystoœci:",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),
    mainPanel(
      plotOutput("smoothPlot")
    )
  )
))

# shiny::runApp('C:\\Users\\sommerm\\Dropbox\\TWDwR\\shiny')
# to trzeba wpisac w konsoli, zeby dzialalo
