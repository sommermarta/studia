library("dplyr")
library("ggplot2")
library('scales')
library("gridExtra")
load("C:\\Users\\sommerm\\Dropbox\\TWDwR\\PISAeurope.Rda")

#########################################################################################
####################################### DANE ##########################################
#########################################################################################

ramka <- pisa %>%
  select(PV1MATH,PV1READ,SC01Q01,CNT,ST71Q01,W_FSTUWT) %>%
  group_by(CNT,SC01Q01) %>%
  #filter(!is.na(PV1MATH) & !is.na(PV1READ) & !is.na(SC01Q01) & !is.na(CNT) & !is.na(ST71Q01)) %>%
  summarise(math = weighted.mean(PV1MATH, W_FSTUWT, na.rm = TRUE),
            read = weighted.mean(PV1READ, W_FSTUWT, na.rm = TRUE),
            hours=mean(ST71Q01,na.rm=TRUE),
            total = sum(W_FSTUWT)) %>%
  filter(!is.na(SC01Q01))
levels(ramka$SC01Q01) <- c('Publiczna','Prywatna')
ramka$opis <- c("B", "Cz", "G", "F", "f", "UK", "g", "J", "K", "P")
podzialkaread <- pretty(ramka$read)
podzialkamath <- pretty(ramka$math)

N <- 7 # tyle kategorii procentowych: 0%, 100% oraz 5 posrednich
## ---- nazwy kategorii ----
nazwy <- c("0%", "(0%, 20%]", "(20%, 40%]", "(40%, 60%]", "(60%, 80%]",
           "(80%, 100%)", "100%")
## ---- funkcja pomocnicza ----
kategoryzuj <- function(x) {
  sapply(x, function(y) {
    if (is.na(y)) return(NA)
    if (y == 1) return(N)
    return(min(which(y <= seq(0, 1, length=N-1))))
  })
}

## ---- przygotowanie danych ----

bg <- pisa %>%
  select(PV1MATH, PV1READ, W_FSTUWT, PCGIRLS, GENDER=ST04Q01) %>%
  filter(!is.na(PCGIRLS)) %>%
  mutate(czysz = kategoryzuj(PCGIRLS)) %>%
  group_by(czysz, GENDER) %>%
  summarise(math=weighted.mean(PV1MATH, W_FSTUWT),
            read=weighted.mean(PV1READ, W_FSTUWT)) %>%
  filter((czysz != 1 | GENDER != "Female") &
           (czysz != N | GENDER != "Male"))

#########################################################################################
#################################### WYKRESY ##########################################
#########################################################################################

#### wykres 1 ########### matematyka_zajecia


(matematyka_zajecia <- ggplot(ramka,aes(x=math,y=hours,color=SC01Q01,shape=CNT))+
   geom_point(size=8)+
   guides(color = guide_legend(title = "Rodzaj szko³y"))+
   scale_shape_manual(values=ramka$opis,name="Pañstwa")+
   scale_colour_manual(
     values = c("Publiczna" = '#990000' ,"Prywatna" = "#5abba7"))+
   #scale_y_reverse()+
   #scale_x_continuous(breaks=c(0,podzialkamath))+
   theme(panel.background = element_blank(),
         legend.position="left",
         axis.line = element_line(color = 'black'),
         axis.title.x = element_text(colour="grey20",size=15),
         axis.title.y = element_text(colour="grey20",size=15),
         title=element_text(colour="grey20",size=20),
         panel.grid.minor=element_line(color='orange'),
         panel.grid.major=element_line(color='#EEDC82'),
         axis.text.y=element_text(size=15),
         axis.text.x=element_text(size=15))+
   labs(list(x="Wyniki z matematyki",y="Liczba zajêæ w tygodniu (h)"))
)
