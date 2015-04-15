library(shiny)
library(Hmisc)
library(ggthemes)
library(dplyr)
library(ggplot2)

load("imiona_warszawa.rda")

shinyServer(function(input, output) {
   
  output$errorbarPlot <- renderPlot({
   
      imiona <- imiona_warszawa 
      
      imiona1 <- imiona %>% 
         group_by(imie) %>% 
            summarise(suma=sum(liczba))
      
      imiona1 <- imiona1 %>% filter(suma < 50)
      
      imiona2 <- imiona %>% 
         filter(imiona$imie %in% imiona1$imie) %>%
            group_by(rok,imie) %>% 
               summarise(suma=sum(liczba))
      
      imiona2$imie <- as.character(imiona2$imie)
      Encoding(imiona2$imie) <- "UTF-8"
      as.factor(imiona2$imie)
  
      imiona3 <- imiona2 %>% 
         filter(imie %in% c(input$imionaa))

      
      ggplot(imiona3, aes(x=rok, y=suma, col=imie))+
         geom_point(size=4,alpha=0.4)+
         geom_line()+
         theme(
            panel.background = element_blank(),
            axis.line = element_line(color = 'black'),
            axis.title.x = element_text(colour="grey20", size = 20),
            axis.title.y = element_text(colour="grey20", size = 20)
            )+
         labs(list(x='Rok', y='Liczba dzieci o tym imieniu'))+
         scale_y_continuous(breaks=0:max(imiona3$suma))
  })
})
  
# komendy pomocnicze:

# levels(imiona2$imie)
# levels(factor(imiona2$imie)) -> wektor_imion
# fix(wektor_imion) 
  
  