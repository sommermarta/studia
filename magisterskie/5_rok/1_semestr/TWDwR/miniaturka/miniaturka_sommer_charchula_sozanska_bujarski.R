############################################################################
# autorzy: Marta Sommer, Marta Charchu³a, Barbara Sozañska, Marcin Bujarski
# 07.01.2015r.- projekt miniaturka
############################################################################

################ opis dzialania funkcji ################

# Funkcja miniaturka() przyjmuje jeden argument - obiekt klasy lm. 
# W przeciwnym przypadku zwraca b³¹d. Funkcja zwraca dwa wykresy 
# (umieszczone na jednym obszarze). Pierwszy z nich to wykres s³upkowy
# przedstawiaj¹cy wspó³czynnik R^2 Adjusted dla ró¿nych metod selekcji
# zmiennych. Drugi natomiast pokazuje, które zmienne z modelu zosta³y 
# wybrane przez poszczególne metody.

# Biblioteki niezbêdne do u¿ycia funkcji miniaturka():
# library("ggplot2")
# library("stringi")
# library("reshape2")
# library("scales")
# library("gridExtra")
# library("extrafont")

#################### funkcja ################################

miniaturka <- function(pel){
   
   if (class(pel)!="lm") stop("Argument musi byc klasy lm!")
   
   # biblioteki:
   
   library("ggplot2")
   library("stringi")
   library("reshape2")
   library("scales")
   library("gridExtra")
   library("extrafont")
   
   # PIERWSZY WYKRES:
   
   # przygotowanie danych:
   
   nobs <- length(pel$residuals)
   form <- pel$call
   ff <- stri_extract_all_regex(as.character(pel$call)[2],"^(.+)[~]")[[1]]
   pus <- lm(paste(ff,1),data=pel$model)

   # dopasowanie metodoa aic, bic, ...:
   
   fa <- step(pus,direction="forward",scope=list(lower=~1,upper=pel), 
              trace=0)
   fb <- step(pus,direction="forward",scope=list(lower=~1,upper=pel),
              k=log(nobs), trace=0)
   ba <- step(pel,direction="backward", trace=0)
   bb <- step(pel,direction="backward", k=log(nobs), trace=0)
   

   r_adj <- c(summary(pel)$adj.r.squared, summary(fa)$adj.r.squared,
              summary(fb)$adj.r.squared, summary(ba)$adj.r.squared,
              summary(bb)$adj.r.squared)*100
   
   r_adj_sort <- sort(r_adj,decreasing = TRUE)
   porzadek <- order(r_adj,decreasing = TRUE)
   
   # wyciagniecie wielkosci modelu:
   
   wyciagnijmodel <- function(m){
      a <- paste(m[1],'~')
      if(length(m)==1){
         a <- paste(a,1)
      } else{
         for(i in 2:length(m)){
            if(i==2){
               a <- paste(a,m[i])
            } else{
               a <- paste(a,'+',m[i])   
            }
         }         
      }
      a
   }
   
   m_pel <- wyciagnijmodel(as.character(attr(pel$terms, "variables"))[-1])
   m_fa <- wyciagnijmodel(as.character(attr(fa$terms, "variables"))[-1])
   m_ba <- wyciagnijmodel(as.character(attr(ba$terms, "variables"))[-1])
   m_fb <- wyciagnijmodel(as.character(attr(fb$terms, "variables"))[-1])
   m_bb <- wyciagnijmodel(as.character(attr(bb$terms, "variables"))[-1])
   
   mod <- c(m_pel,m_fa,m_fb,m_ba,m_bb)
   mod_sort <- mod[porzadek]
   
   # kosmetyka dotyczaca wyswietlania:
   
   metody <- c('Model\npe³ny', 'Forward\nAIC', 'Forward\nBIC', 
               'Backward\nAIC', 'Backward\nBIC')
   metody_sort <- metody[porzadek]
   
   to <- data.frame(metody_sort, r_adj_sort)
   toto <- melt(to,"metody_sort")
   
   toto$metody_sort <- factor(toto$metody_sort,levels=metody_sort)
   
   kol <- rep("darkolivegreen1",5)

   # ostateczny wykres nr 1:
   
   wyk1 <- ggplot(toto, aes(x=metody_sort, y=value/100, 
                            label=paste(round(value),"%",sep="")))+
      geom_bar(stat="identity", color="black", fill=kol)+
      geom_text(hjust=1.2)+
      coord_flip()+
      theme_bw()+
      ggtitle(expression(atop(WSPÓ£CZYNNIK~italic(R^2~-~ADJUSTED),DLA~RÓ¯NYCH~METOD~SELEKCJI~ZMIENNYCH)))+
      theme(axis.text = element_text(colour="grey20",size=16,face="plain",
                                     family="Times New Roman"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(size=18,family="Times New Roman"),
            panel.grid.minor.x=element_blank(), 
            panel.grid.major.x=element_blank())+
      scale_y_continuous(labels=percent)

   # DRUGI WYKRES:

   # przygotowanie danych:

   zm_pel <- as.character(attr(pel$terms, "variables"))[-c(1,2)]
   zm_fa <- as.character(attr(fa$terms, "variables"))[-c(1,2)]
   zm_ba <- as.character(attr(ba$terms, "variables"))[-c(1,2)]
   zm_fb <- as.character(attr(fb$terms, "variables"))[-c(1,2)]
   zm_bb <- as.character(attr(bb$terms, "variables"))[-c(1,2)]

   zm_macierz <- matrix(0,ncol=length(zm_pel)+1,nrow=5)
   zm_macierz[1,] <- 1

   miejsca <- function(zzz){
      wek <- numeric(length(zm_pel))
      if(length(zzz)==0){
         wek <- 0
      } else{
         for(i in 1:length(zzz)){
            for(j in 1:length(zm_pel)){
               if(zm_pel[j]==zzz[i]){
                  wek[j] <- 1   
               }
            } 
         }
         c(wek,0)      
      }  
   }

   zm_macierz[2,] <- miejsca(zm_fa)
   zm_macierz[3,] <- miejsca(zm_fb)
   zm_macierz[4,] <- miejsca(zm_ba)
   zm_macierz[5,] <- miejsca(zm_bb)
   zm_macierz[,length(zm_pel)+1] <- metody

   zm_macierz <- zm_macierz[porzadek,]
   zm_dataframe <- as.data.frame(zm_macierz)
   colnames(zm_dataframe) <- c(zm_pel,"metody")
   suppressWarnings(dane <- melt(zm_dataframe, "metody"))
   dane$metody <- factor(dane$metody,levels=metody[porzadek])

   # ostateczny wykres nr 2:

wyk2 <- ggplot(dane, aes(x=variable, y=metody, fill=value))+
   geom_tile(colour="black")+
   scale_fill_manual(values=c("white","gray75"),
                     labels=c("Nie nale¿y\ndo modelu","Nale¿y\ndo modelu"))+
   theme_bw()+
   theme(axis.text = element_text(colour="grey20", size=16, face="plain", 
                                  family="Times New Roman"),
         axis.title = element_blank(),
         plot.title=element_text(size=18,family="Times New Roman"),
         panel.grid.minor=element_blank(),
         legend.title=element_blank())+
   ggtitle("PRZYNALE¯NOŒÆ ZMIENNYCH DO MODELU")

# wykresy razem:

grid.newpage() 
print(wyk1, vp=viewport(x=0.5, y = 0.75, width=1, height=0.5))
print(wyk2, vp=viewport(x=0.5, y = 0.25, width=1, height=0.5))

}

#################### przyklady ################################

# 1

cr <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/uscrime.txt",
                 header=T)
pel <- lm(R~., data=cr)
miniaturka(pel)

# 2

r <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/realest.txt",
                header=TRUE)
pel <- lm(r$Price~., data=r)
miniaturka(pel)

# 3
s <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/savings.txt",
                header=T)
pel <- lm(Savings~.-Country, data=s)
miniaturka(pel)

# 4
p <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/prostate.data",
                header=TRUE)
pel <- lm(lpsa~.,data=p)
miniaturka(pel)

# 5

miniaturka(56)

# 6

library("MASS")
miniaturka(lda(Savings~.-Country, data=s))

###################### pomocnicze rzeczy: ###############################

# instalacja czcionek:

# install.packages("extrafont");
# library(extrafont)
# font_import(pattern="[C/c]omic")
# font_import(pattern="[A/a]rial")
# font_import(pattern="[T/t]imes")
# font_import(pattern="[H/h]elvetica")
# y
# fonts()
# fonttable()
# loadfonts(device="win")
