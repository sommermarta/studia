library("dplyr")
library("ggplot2")
library('scales')
library("gridExtra")
load("C:\\Users\\sommerm\\Dropbox\\TWDwR\\shiny\\shiny1_my\\niewiem.Rda")

# wykres shiny

shinyServer(function(input, output) {
    
  output$smoothPlot <- renderPlot({
  
    ramka2 <- ramka %>%
      filter(CNT %in% c(input$kraje))
    
    p <- ggplot(ramka2,aes(x=math,y=hours,color=SC01Q01,shape=CNT))+
      
      geom_point(size=8)+
      geom_line(arrow=arrow())+
      guides(color = guide_legend(title = "Rodzaj szko³y"))+
      scale_shape_manual(values=ramka$opis,name="Pañstwa")+
      
      scale_colour_manual(
        values = c("Publiczna" = input$colpub ,"Prywatna" = input$colpryw))+
      
      scale_x_continuous(breaks=round(seq(min(ramka$math),max(ramka$math), by=input$gestoscx)))+
      #scale_y_continuous(breaks=round(seq(min(ramka$hours),max(ramka$hours), by=0.1)), limits=input$limits)+
      
      theme(panel.background = element_blank(),
            legend.position="right",
            axis.line = element_line(color = 'black'),
            axis.title.x = element_text(colour="grey20", size = input$rozmiar),
            axis.title.y = element_text(colour="grey20", size = input$rozmiar),
            title=element_text(colour="grey20",size=20),
          #  panel.grid.minor=element_line(color='orange'),
          #  panel.grid.major=element_line(color='#EEDC82'),
            text = element_text(size = input$rozmiar, family = input$czcionka))+
      
      labs(list(x="Wyniki z matematyki",y="Liczba zajêæ w tygodniu (h)"))+
      ylim(c(input$limits[1],input$limits[2]))
    
    return(if( !input$legenda ){ p+ guides(colour=input$legenda, shape=input$legenda)} else {p})
  })
})

# shiny::runApp('C:\\Users\\sommerm\\Dropbox\\TWDwR\\shiny\\shiny1_my')




