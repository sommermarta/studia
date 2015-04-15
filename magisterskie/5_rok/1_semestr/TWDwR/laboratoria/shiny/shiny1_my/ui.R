
shinyUI(fluidPage(
  
  titlePanel("Wyniki z matematyki a liczba godzin w tygodniu i kraj pochodzenia"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("gestoscx",
                  "Gestosc siatki OX:",
                  min = 1,
                  max = 10,
                  value = 10), # domyslna wartosc poczatkowa
#       sliderInput("gestoscy",
#                   "Gestosc siatki OY:",
#                   min = 0.5,
#                   max = 3,
#                   value = 1, step=0.5),
#       
      sliderInput("limits", label = "Granice dla OY",
                  min = 0, max = 40, value = c(24, 36)),
      
      selectInput("czcionka", label = "Wybierz czcionkê na wykresie",
                  choices = list("mono", "serif", "sans"),
                  selected = "mono"),
      
      sliderInput("rozmiar",
                  "Rozmiar czcionki:",
                  min = 1,
                  max = 40,
                  value = 16),
      
      checkboxInput("legenda", label = "Pokazuj legendê", value = FALSE),
      
      selectInput("colpub", label="Kolor dla szkoly publicznej:",
                  choices=c("red","green","blue","yellow","black","pink"), selected="red"),
      
      selectInput("colpryw", label="Kolor dla szkoly prywatnej:",
                  choices=c("red","green","blue","yellow","black","pink"), selected="blue"),
      
      checkboxGroupInput("kraje",
                         label = h3("Jakie kraje?"),
                         choices = list("Belgium", "Czech Republic", "Germany", "Finland", "France",
                                        "United Kingdom", "Greece", "Japan", "Korea", "Poland"),
                         selected = list("Belgium", "Czech Republic", "Germany", "Finland", "France",
                                         "United Kingdom", "Greece", "Japan", "Korea", "Poland"))
    ),
    mainPanel(
      p("Oto piêkny wykres"),
      plotOutput("smoothPlot", width=1200, height=800)
    )
  )
))


# shiny::runApp('C:\\Users\\sommerm\\Dropbox\\TWDwR\\shiny')
# to trzeba wpisac w konsoli, zeby dzialalo

# fis() -> zeby ladny wektor fajnie zapisac




