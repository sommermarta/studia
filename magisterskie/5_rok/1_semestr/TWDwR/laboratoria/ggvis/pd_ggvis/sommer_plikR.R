library("tidyr")
library("ggvis")
library("dplyr")
library("ggplot2")


setwd("C:\\Users\\Marta\\Dropbox\\TWDwR\\laboratoria\\ggvis" )
load("PISAeurope.rda")

# przygotowanie danych:

dane <- data.frame(pisa$CNT, pisa$ST04Q01, pisa$PV1MATH, pisa$ESCS, 
                   pisa$PV1READ, pisa$CULTPOS, pisa$HISEI, 
                   pisa$W_FSTUWT)

names(dane) <- c("kraj", "plec","matma", "ekonomicznyindeks", 
                 "czytanie", "dobrakultury", "statusrodzicow", 
                 "wagi")

dane2 <- dane %>%
   group_by(kraj, plec) %>% 
   summarise(math = weighted.mean(matma, wagi, na.rm = TRUE),
             read = weighted.mean(czytanie, wagi, na.rm = TRUE))  


dane3 <- dane2 %>%
   group_by(kraj) %>% 
   arrange(kraj,plec) %>%
   summarise(Math = diff(math),
             Reading = diff(read)) 


dane4 <- gather(dane3, key=kraj)
names(dane4) <- c("kraj", "Test", "roznicaliczba")

# wykres:

dane4 %>%
   ggvis(x = ~kraj, 
         y = ~roznicaliczba, 
         fill= ~Test) %>%
   layer_points(size := 300, 
                opacity := 0.8) %>%
   layer_lines(y=0) %>%
   add_axis("x", 
            title = "Country", 
            title_offset = 50) %>%
   add_axis("y", 
            title = "Difference between male and female score",
            title_offset = 50) %>% 
   add_axis("x", 
            orient = "top", 
            ticks = 0,
            subdivide = 0, 
            tick_padding = 0, 
            tick_size_major = 0, 
            title = "MEN BETTER AT MATH AND WOMEN AT READING!", 
            title_offset = 20,
            properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0, 
                             stroke="white"))) 












