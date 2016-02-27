## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 09
##
## Prowadzacy:    Cena Anna
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 09.01 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja kartka(), ktora jako argument przyjmuje napis x zawierajacy 
# zyczenia swiateczne, wypisuje x na obrazku, na tle kojarzacym sie z 
# czasem swiatecznym.
#
# ARGUMENTY WEJSCIOWE
# 
# x - pojedynczy napis
# ... - parametr przekazywany do funkcji text() (mozna dzieki niemu zmienic
#       na przyklad kolor wypisywanego tekstu)
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

kartka <- function(x=stri_paste("Szczêœliwego Nowego Roku, du¿o",
               " pomyœlnoœci, radoœci i b³ogos³awieñstwa bo¿ego!!!"),...){
   
   stopifnot(is.character(x))
   
   kolko <- function(x0=0,y0=0,r=1,pocz=0,kon=2*pi,...){
      teta <- seq(pocz,kon,length.out=1000)
      x <- x0 + r*cos(teta)
      y <- y0 + r*sin(teta)
      polygon(x,y,...)
      lines(x,y)
   }
   
   balon <- function(x1=0,y1=0,c="yellow",r1=1){
      
      polygon(c(x1+r1,x1-r1,x1),c(y1,y1,y1-1.5*r1),col=c)
      kolko(x0=x1,y0=y1,r=r1,col=c,pocz=-0.05,kon=1.05*pi,border=NA)
      kolko(x0=x1+r1/2,y0=y1,r=1.5*r1,col=c,pocz=pi-0.05,
            kon=1.4*pi+0.05,border=NA)
      kolko(x0=x1-r1/2,y0=y1,r=1.5*r1,col=c,pocz=1.6*pi-0.05,
            kon=2*pi+0.05,border=NA)
      points(x1+1/2*r1,y1+1/2*r1,pch=23,bg="white")
      points(x1+6/8*r1,y1+1/8*r1,pch=23,bg="white",cex=0.8)
      
      y <- seq(pi/2,2*pi,length.out=100)
      x <- sin(1/2*y)
      lines(x1+x,y1-1.5*r1+y-2*pi)
      
   }
   
   plot.new()
   plot.window(c(-4,4),c(-6,6))
   polygon(c(-4.32,-4.32,4.32,4.32,-4.32),c(-6.48,6.48,6.48,-6.48,-6.48),
           col="snow1")
   
   balon(x1=-2,y1=2,c="green",r=1.4)
   balon(x1=-1,y1=-2,c="red",r=1.5)
   balon(x1=3,y1=3,c="pink",r=1.2)
   balon(x1=2,y1=-1,c="blue",r=1.3)
   balon(x1=-3,y1=3,c="orange",r=1)
   balon(x1=0,y1=5,c="purple",r=1.35)
   balon(x1=1.5,y1=1,c="gold")
   balon()
   
   a <- unlist(stri_split_regex(x," "))
   g <- seq(6,-6,length.out=length(a))
   
   for(i in 1:length(a)){
      text(0,g[i],a[i],cex=1.2,srt=5,...)
      text(0,g[i],a[i],cex=1.2,srt=5,...)
      text(0,g[i],a[i],cex=1.2,srt=5,...)
      text(0,g[i],a[i],cex=1.2,srt=5,...)
      text(0,g[i],a[i],cex=1.2,srt=5,...)
   }
}


## ---- Przyklady ----

kartka()
kartka(col="brown")


## ------------------------ Zadanie 09.02 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja choinka() rysuje moja wlasna choinke matematyczna (jest to choinka
# hiperboliczna i w ogole nie jest smieszna :D ale niestety nie mialam
# zadnego kreatywnego pomyslu).
#
# ARGUMENTY WEJSCIOWE
# 
# ... - parametr przekazywany do funkcji text() (mozna dzieki niemu zmienic
#       na przyklad kolor wypisywanego tekstu)
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

choinka <- function(...){
   
   z <- seq(-1,1,length.out=40)
   yy <- abs(1/z)
   
   plot.new()
   plot.window(c(-1,1),c(0,max(yy)+5))
   
   polygon(c(z,z[1]),c(yy,yy[1]),col="green",border=NA)
   
   lines(z,yy,type="b",pch=16,cex=1.2,col=c("green1","green2","green3","green4"))
   points(0,max(yy)+5,pch=8,col="yellow",cex=3)
   
   text(-1/2,(max(yy)+5)/2+5,"Choinka",...)
   text(-1/2,(max(yy)+5)/2,"hiperboliczna",...)
   
}

## ---- Przyklady ----

choinka()
choinka(col="red")

## ------------------------ Zadanie 09.03 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja bombka() rysuje wykres kolowy dla danego zbioru danych x, 
# odpowiednio go zmniejsza i umieszcza na obrazku. Obiekt ten ma pelnic
# role jednej z babek na choince.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor liczbowy
# ... - parametr przekazywany do funkcji pie()
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

bombka <- function(x,...){
   
   as.character(is.numeric(x))
   
   circle <- function(x0=0,y0=0,r=1,n=1000,...){
      theta <- seq(0,2*pi,length.out=n)[-1]
      x <- x0 + r*cos(theta)
      y <- y0 + r*sin(theta)
      polygon(x,y,...)
   }
   
   tr <- function(x0=0,y0=0,a=0.5,...){
      polygon(c(x0,x0+a/2,x0-a/2),c(y0,y0-a*sqrt(3)/2,y0-a*sqrt(3)/2),...)
   }
   
   lancuch <- function(x0,y0,x1,y1,n=20){
      x <- seq(x0,x1,length.out=n)
      a <- (y1-y0)/(x1-x0)
      b <- (x1*y0-y1*x0)/(x1-x0)
      y <- a*x+b
      points(x,y,type="p",pch=8,col="gold")
   }
   
   plot.new()
   plot.window(c(-1,1),c(-1,1))
   
   rect(-0.1,-1,0.1,0,col="brown4")
   
   tr(0,0.4,1.2,col="green4")
   tr(0,0.6,0.9,col="green3")
   tr(0,0.8,0.6,col="green2")
   tr(0,0.95,0.3,col="green")
   
   lancuch(-0.23,0.5,0.14,0.7)
   lancuch(-0.23,0.5,0.35,0.1)
   lancuch(0.35,0.1,-0.5,-0.4)
   lancuch(-0.5,-0.4,0.6,-0.64)
   
   par(new=TRUE)
   
   pie(x,radius=0.1,labels=NA,...)
   
   circle(x0=-0.2,y0=0.2,r=0.1,col="red")
   circle(x0=0.1,y0=0.5,r=0.1,col="pink")
   circle(x0=-0.1,y0=-0.5,r=0.1,col="blue")
   circle(x0=-0.5,y0=-0.6,r=0.1,col="yellow")
   circle(x0=0.3,y0=-0.1,r=0.1,col="gold")
   circle(x0=0.4,y0=-0.5,r=0.1,col="darkred")
   
   points(0,1,pch=25,cex=4,col="yellow",bg="yellow")
   points(0,1,pch=24,cex=4,col="yellow",bg="yellow")
   
}

## ---- Przyklady ----

x <- c(ja=2,ty=10,on=4,wy=55,ini=33,my=33,oni=33)
bombka(x)
bombka(x,col=heat.colors(length(x)))


## ------------------------ Zadanie 09.04 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja prezenty() rysuje wykres funkcji dwoch zmiennych f(x,y) = x + 8*y,
# ktora opisuje prawdopodobienstwo otrzymania wspanialego prezentu pod
# choinke, gdzie x to grzecznosc dziecka w ciagu roku, a y to jego 
# zaangazowanie w nauke PADR-u (widac od razu, ze PADR jest az osiem razy
# wazniejszy niz bycie grzecznym :D).
#
# ARGUMENTY WEJSCIOWE
# 
# ... - parametr przekazywany do funkcji persp() 
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

prezenty <- function(...){
   
   grzecznosc <- 1:10
   zaangazowanie <- 1:10
   
   y <- outer(grzecznosc,zaangazowanie,function(x,y){
      x + 8*y 
   })
   
   persp(grzecznosc, zaangazowanie, y,col="#ff0000da",
         xlab="grzecznoœæ dziecka", ylab="zaanga¿owanie w PADR", 
         zlab="p-stwo dostania prezentu",...)
   
}

## ---- Przyklady ----

prezenty()
prezenty(border=NA)


## ------------------------ Zadanie 09.05 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# Funkcja gazetowo() odtwarza wykres zamieszczony w grudniowym wydaniu 
# gazety (oryginal przesylam na maila). Zmienilam w nim troche legende
# (bo w oryginale byly tam loga partii politycznych, a ja zrobilam napisy). 
# Inne sa tez nieco procenty (bo wedlug mnie w gazecie byly nieco oszukane).
#
# ARGUMENTY WEJSCIOWE
# 
# ... - argument przekazywany do funkcji legend() (mozna na przyklad 
#       ustalic, czy legenda bedzie w ramce, czy nie)
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

gazetowo <- function(...){
   
   ile <- c(148,17,55,48,48,130,2,4,6,2)
   
   nazwy <-c("PO","PiS","SLD","PSL","Nowa Prawica","Ruch Poparcia Palikota",
             "Solidarna Polska","Zieloni 2014","Ruch Narodowy","Inne") 
   
   kolory <- c("blue","darkblue","red","green","turquoise","orange",
               "darkblue","darkgreen","red4","black")
   
   k <- rep(kolory,ile)
   
   plot.new()
   plot.window(c(-1,1),c(-1,1))
   
   s <- 0
   w <- 0
   theta <- seq(pi,0,length.out=46)
   er <- seq(0.5,1,length.out=10)
   
   for(i in 1:length(theta)){
      for(r in 1:length(er)){      
         
         x <- er[r]*cos(theta[i])
         y <- er[r]*sin(theta[i])
         
         lines(c(0,x),
               c(0,y),type="p",pch=16,col=k[w+r],cex=1.2)
         
      }
      w <- w+r
   }
   
   points(0,0,col="white",cex=2,pch=16)
   
   
   a <- stri_paste(round(ile/460*100,2)," %")
   b <- stri_paste(nazwy," - ")
   c <- stri_paste(b,a)
   
   legend("bottom",c,col=kolory,pch=16,cex=0.73,...)
   
}

## ---- Przyklady ----

gazetowo()
gazetowo(bty="n")

