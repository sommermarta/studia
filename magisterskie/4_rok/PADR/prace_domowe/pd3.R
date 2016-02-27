## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 03
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 03.07 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja rle2() jako argument przyjmuje wektor x i zwraca liste skladajaca 
# sie z dwoch wektorow nazwanych: lengths i values, z czego lengths 
# pokazuje, ile tych samych elementow pod rzad znajduje sie w wektorze x, a 
# values odpowiednio, jakie sa te elementy. Ewentualne wartosci NA sa przez
# funkcje usuwane z wektora x.
#
# ARGUMENTY WEJSCIOWE
#
# x - wektor atomowy
#
# WARTOSC ZWRACANA
#
# lista klasy rle, skladajaca sie z dwoch wektorow nazwanych: 
# lengths i values
#

## ---- Funkcja ----

rle2 <- function(x){
   
   stopifnot(is.atomic(x))    # x - wektor atomowy
   stopifnot(length(x)>0)     # x nie moze byc pusty
   
   x <- na.omit(x)
   l <- numeric(length(x))
   v <- numeric(length(x))
   s <- 1
   a <- 0
   
   for(i in 1:length(x)){
      
      if(i==length(x)){
         
         v[i-a] <- x[i-s+1]
         l[i-a] <- s
         
      } else{
         
         if(x[i]==x[i+1]){
            
            s <- s+1
            a <- a+1
            
         } else{
            
            if(s==1){
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- 1
               
            } else{
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- s
               
            }
            
            s <- 1
         }
      }
   }
   
   c <- length(x)-a+1    #tu usuwamy niepotrzebne koncowe elementy z v i l 
   
   if(!(c>length(x))){
      v <- v[-(c:length(v))]
      l <- l[-(c:length(l))]
   }
   
   rle2 <- list(lengths=l,values=v)
   class(rle2) <- "rle"
   rle2
   
}

## ---- Przyklady ----

# wiekszosc przykladow bede porownywac z funkcja rle() 
# - jesli jest ok, to powinno wychodzic TRUE

x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,9,4)
r <- rle(x)
r2 <- rle2(x)  
all(c(r$lengths==r2$lengths,r$values==r2$values))==TRUE    #zgadza sie         


x <- c(2,4,6,8)
r <- rle(x)
r2 <- rle2(x)  
all(c(r$lengths==r2$lengths,r$values==r2$values))==TRUE 

x <- c()
rle2(x)     #error - wektor pusty

x <- c(2,4,6,8,NA)
rle2(x)

x <- c(2,4,NA,6,8)
rle2(x)



## ------------------------ Zadanie 03.08 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Stworzylam dwa rozwiazania tego zadania - jedno bez petli (is.prime()), a 
# drugie z petla (is.prime2()). Obie funkcje dla danego wektora liczb 
# naturalnych zwracaja wektor logiczny, ktorego i-ty element odpowiada na 
# pytanie, czy i-ty element wektora x jest liczba pierwsza (przyjmuje 
# oczywiscie, ze 1 nie jest liczba pierwsza). Gdy w wektorze pojawia sie NA,
# to funkje zwracaja blad.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor liczb naturalnych {1,2,3,...} 
#
# WARTOSC ZWRACANA
# 
# wektor logiczny dlugosci wektora x 
#



## ---- Funkcja ----


is.prime <- function(x){        #funkcja bez uzycia petli
   
   stopifnot(is.numeric(x))    #czy jest liczbowy
   stopifnot(all(x>=1))        #czy jest wiekszy rowny zero
   stopifnot(x==floor(x))      #czy jest naturalny
   
   f <- function(i){            #tworze funkcje pomocnicza, ktora zwraca 
                                #wartosc TRUE, gdy element x jest podzielny 
                                #przez ktoraz z liczb {2,...,x-1} - wtedy x 
                                #nie jest liczba piersza
      
      any( ( (x[i] %% 2:(x[i]-1) ) == 0 ))
      
   }
   
   !unlist(lapply(1:length(x),f))
}



is.prime2 <- function(x){      #inne rozwiazanie z uzyciem petli
   
   stopifnot(is.numeric(x))    #czy jest liczbowy
   stopifnot(all(x>=1))        #czy jest wiekszy rowny zero
   stopifnot(x==floor(x))      #czy jest naturalny
   
   a <- logical(length(x))
   
   for(i in (1:length(x))){
      
      if( any( ( (x[i] %% 2:(x[i]-1) ) == 0 )) ){
         
         a[i] <- FALSE
         
      } else{
         
         a[i] <- TRUE
      }
   }
   
   a
}

## ---- Przyklady ----

x <- c(3,4,7,7,11,4,5,25,31,30,1)
is.prime(x)
is.prime2(x)

x <- c(-3,4,7,7,11,4,5,25,31,30,1)
is.prime(x)    #error - to nie liczby naturalne
is.prime2(x)

x <- c(3.5,4,7,7,11,4,5,25,31,30,1)
is.prime(x)    #error - to nie liczby naturalne
is.prime2(x)

x <- c()
is.prime(x)    #error - wektor pusty
is.prime2(x)

x <- c("hhdhhd")
is.prime(x)    #error - wektor nie jest numeryczny
is.prime2(x)

x <- c(3,NA,4,7,7,11,4,5,25,31,30,1)
is.prime(x)    #error - gdy pojawia sie NA
is.prime2(x)


fun <- function(n){
   n^2 + n + 41
}

x <- fun(0:39)
is.prime(x)  #dziala - same TRUE :)         
is.prime2(x)


## ------------------------ Zadanie 03.09 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja onp(), ktora jako argument przyjmuje wektor napisow x, zlozony z
# odzielnych tokenow wyrazenia zapisanego w postaci ONP, zwraca wynik 
# dzialania tego dzialania. 
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow
#
# WARTOSC ZWRACANA
#
# wektor liczbowy jednowymiarowy (wartosc liczbowa)
#


## ---- Funkcja ----

onp <- function(x){
   
   stopifnot(is.character(x))     #czy x jest wektorem napisow
   stopifnot(length(x)%%2!=0)     #czy jest nieparzyscie elementow
   stopifnot(sum(is.na(as.numeric(x)))==(length(x)-1)/2)   #czy jest o jeden 
   #wiecej liczb niz 
   #dzialan
   
   s <- numeric()
   a <- 0
   
   for(i in 1:length(x)){
      
      if(is.na(as.numeric((x[i])))==FALSE){
         s[i-2*a] <- as.numeric(x[i])
      } else{
         f <- get(x[i])
         s[i-2*a] <- f(s[i-2*a-2],s[i-2*a-1])
         s <- s[-c(i-2*a-1,i-2*a-2)]
         a <- a+1
      }   
   }
   
   s
   
}

## ---- Przyklady ----

x <- c("1","2","+","3","*","4","5","+","6","7","+","/","-")
onp(x)   #ok :)

x <- c(1,2,3,"+","-","+")
onp(x)   #error

x <- c("*","+","-")
onp(x)   #error

x <- c(1,4,"+",5,"/",20,3,"-",6,"*","+",1,"/")
onp(x)  #dziala :)




## ------------------------ Zadanie 03.10 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja ligiderle() jako argumenty przyjmuje rownoliczne wektory 
# calkowitoliczbowe i,j oraz wartosc calkowita i. Funkcja zwraca blad, gdy
# dla jakiegos l nie zachodzi 1 <= i[l] <= j[l] <= n oraz i[l] > j[l-1].  
# W przeciwnym przypadku funkcja generuje n elementowy wektor logiczny w 
# taki, ze w[l]==TRUE wtedy i tylko wtedy, gdy istnieje p takie, ze l nalezy
# do przedzialu [i[p],j[p]].
#
# ARGUMENTY WEJSCIOWE
# 
# n - wartosc calkowita
# i - wektor calkowitoliczbowy
# j - wektor calkowitoliczbowy dlugosci tej samej, co wektor i 
#
# WARTOSC ZWRACANA
#
# n elementowy wektor logiczny
#

## ---- Funkcja ----

logiderle <- function(i,j,n){
   
   stopifnot(is.numeric(i) & is.numeric(j))  # i i j - numeryczne
   stopifnot(floor(i)==i & floor(j)==j)   # i i j - caklowite
   stopifnot(length(i)==length(j))  # i i j tej samej dlugosci 
   stopifnot(n>=1 & floor(n)==n)   # n - naturalne 
   stopifnot(all(i>=1) & all(i<=j) & all(j<=n)  # ten dlugi warunek z 
             #dokumentacji :)
             & all(i[2:length(i)] > j[1:length(j)-1]))
   
   
   fun <- function(l){ 
      c(i[l],j[l])
   }
   
   li <- length(i)
   a <- lapply(1:li,fun)   
   
   fff <- function(k){
      l <- 1:n
      l>=a[[k]][1] & l<=a[[k]][2] 
   }
   
   m <- lapply(1:li,fff)
   q <- unlist(m)
   
   t <- logical(n)
   w <- which(q==TRUE) %% n
   w[which(w==0)] <- n
   t[w] <- TRUE
   t
   
}

## ---- Przyklady ----

i <- c(1,4)
j <- c(1,6)
n <- 7
logiderle(i,j,n)   #zgadza sie :)

i <- c(1,4,9)
j <- c(1,6)
n <- 7
logiderle(i,j,n)   #error - dlugosci sie nie zgadzaja

i <- c(1,7)
j <- c(1,6)
n <- 7
logiderle(i,j,n)   #error, bo 7 nie jest wieksze od 6

i <- c(1,4,8)
j <- c(1,6,8)
n <- 7
logiderle(i,j,n)   #error

i <- c(1,NA)
j <- c(1,6)
n <- 7
logiderle(i,j,n)   # przy wartosciach NA wyskoczy error :)

i <- c()
j <- c()
n <- 7
logiderle(i,j,n)   # przy pustych wektorach wyskoczy blad :)

i <- c(1,4)
j <- c(1,6)
n <- 7.5
logiderle(i,j,n)   # n nie jest naturalne - error

i <- c(1,4)
j <- c(1,6.5)
n <- 7
logiderle(i,j,n)   # j nie jest calkowitoliczbowe - error



## ------------------------ Zadanie 03.11 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# Funkcja uzubraki() (oraz uzubraki2()), ktora jako argument przyjmuje 
# wektor liczbowy x, zwraca wektor powstaly przez uzupelnienie brakow 
# danych w x nastepujacy sposob:
# - jesli NA pojawiaja sie na skrajnych indeksach x, to zastepujemy je 
#   odpowiednio pierwsza lub ostatnia wartoscia niebrakujaca,
# - jesli NA pojawiaja sie wewnatrz wektora, to stosujemy liniowa 
#   interpolacje sasiadujacych po obydwu jej stronach, dobrze okreslonych
#   elementow.
#
# ARGUMENTY WEJSCIOWE
#
# x - wektor liczbowy, taki, ze gdybysmy usuneli z niego braki danych,
#     bylby posortowany rosnaco
#
# WARTOSC ZWRACANA
# 
# wektor liczbowy dlugosci tej samej co x, bez brakow danych
#

## ---- Funkcja ----

uzubraki <- function(x){    #funkcja bez uzycia petli
   
   stopifnot(is.numeric(x))                  # x liczbowy
   stopifnot(sort(na.omit(x))==na.omit(x))    # x posortowany
   
   lx <- length(x)
   a <- as.numeric(is.na(x))
   r <- rle(a)
   rl <- r$lengths
   rv <- r$values
   lr <- length(rl)
   
   a1 <- numeric()
   a2 <- numeric()
   
   A <- numeric()
   B <- numeric()
   
   if(rv[1]==1){
      a1 <- 1:rl[1]
      A[a1] <- x[rl[1]+1]
      
   } 
   
   if(rv[lr]==1){
      a2 <- lx:(lx-rl[lr]+1)
      B[1:(rl[lr]+1)] <- x[lx-rl[lr]]
   }  
   
   a22 <- sort(a2)
   if(length(a1)+length(a22)==0) xx <- x else xx <- x[-c(a1,a22)]
   
   w <- which(as.numeric(is.na(rle(xx)$values))==0)
   lw <- length(w)
   
   f <- function(i){
      c(xx[w[i]],xx[w[i+1]])
   }
   
   przedz <- lapply(1:(lw-1),f)
   
   itjn <- diff(w)
   
   ff <- function(i){
      s <- seq(przedz[[i]][[1]],przedz[[i]][[2]],length.out=itjn[i]+1)
      s[-length(s)]
   }
   
   ost <- lapply(1:length(przedz), ff)
   ostt <- unlist(ost)
   
   if(length(B)==0) round(c(A,ostt,x[lx]),2) else round(c(A,ostt,B),2)
   
}

uzubraki2 <- function(x){    #rozwiazanie alternatywne (z petla)
   
   stopifnot(is.numeric(x))                  # x liczbowy
   stopifnot(sort(na.omit(x))==na.omit(x))    # x posortowany
   
   a <- as.numeric(is.na(x))
   s <- 1
   lx <- length(x)
   u <- numeric(lx)
   ra <- rle(a)
   rl <- ra$lengths
   lrl <- length(rl)
   rv <- ra$values
   
   if(rv[1]==1) k <- rl[1] else k <- 1  #warunek potrzebny, zeby wiedziec 
                                        #ile jest wartosci NA na poczatku
                                        #wektora x
   
   for(i in 1:lx){
      
      if(i==1){             #przypadek, gdy jestesmy w pierwszym elemencie x
         if(a[1]==0) u[1] <- x[1] else{
            w <- rep(x[1+rl[1]],rl[1])
            u[1:rl[1]] <- w
         }
      }
      
      if(i==lx){          #przypadek, gdy jestesmy w ostatnim elemencie x
         if(a[i]==0) u[i] <- x[i] else{
            w <- rep(x[i-rl[lrl]],rl[lrl])
            u[length(x):(length(x)-rl[lrl]+1)] <- w
         }
      }
      
      if(all(i!=1:k & i!=lx)==TRUE){   #pozostale przypadki
         if(a[i]==0){
            u[i] <- x[i] 
         }  else{
            if(a[i+1]==1){
               s <- s+1
            } else{
               m <- (x[i+1]-x[i-s])/(s+1)
               mm <- seq(x[i-s],x[i+1],by=m)
               mm <- mm[-c(1,length(mm))]
               u[(i-s+1):i] <- mm
               s <- 1
            }
         }
      }
   }
   
   round(u,2)
   
}

## ---- Przyklady ----

x <- c(1,NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13)
uzubraki(x)  #dziala, gdy NA nie ma na poczatku ani na koncu
uzubraki2(x)

x <- c(NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13,NA)
uzubraki(x)  #dziala, gdy NA sa na poczatku
uzubraki2(x)

x <- c(NA,2,NA,3,NA,NA,5,10,NA,NA)
uzubraki(x)  #dziala, gdy NA sa na koncu
uzubraki2(x)

x <- c(2,3,5,10,13)
uzubraki(x)  #dziala dla wektora bez brakow danych
uzubraki2(x)

x <- c(2,NA,5,3)
uzubraki(x)  #error - x nie jest posortowany
uzubraki2(x)

x <- c()
uzubraki(x)  #error, gdy x jest pusty
uzubraki2(x)

x <- c(2.79,NA,5,9)
uzubraki(x)  #dziala dla ulamkow
uzubraki2(x)


## ------------------------ Zadanie 03.12 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# funkcja merge2() przujmuje jako argumeny listy x i y, a w wyniku zwraca
# liste powstala przez zlaczenie list danych na wejsciu wzgledem elementow
# nazwanych. Gdy laczone wektory nie maja tych samych typow, wygenerowane 
# zostnie ostrzezenie, ze nastapia koercja typow.
#
# ARGUMENTY WEJSCIOWE
# 
# x - lista nazwana
# y - lista nazwana
#
# WARTOSC ZWRACANA
#
# lista nazwana
#

## ---- Funkcja ----

merge2 <- function(x,y){
   
   stopifnot(is.list(x))
   stopifnot(is.list(y))
   
   nx <- names(x)
   ny <- names(y)
   iu <- intersect(nx,ny)
   u <- union(nx,ny)
   su <- sort(u)
   
   f <- function(i){
      c(x[[i]],y[[i]])
   }
   
   ff <- function(i){           #ta funkcja potrzebna jest do generowanie 
                                #ostrzezenia
      typeof(x[[i]])==typeof(y[[i]])
   }
   
   li <- lapply(su,f)
   names(li) <- su
   
   li2 <- unlist(lapply(iu,ff))
   if(any(li2 == FALSE))  warning("Nastapila koercja typow!")
   
   li
   
}

## ---- Przyklady ----

x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=TRUE)
merge2(x,y)   #dziala, warning tez sie pojawia

x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=c("gg","pola"))
merge2(x,y)   #dziala bez warningu :)

x <- c(8,9)
y <- list(a=4:6, b=7:9, c=TRUE)
merge2(x,y)   #error - x nie jest lista

x <- list()
y <- list(a=4:6, b=7:9, c=TRUE)
merge2(x,y)   #x - lista pusta, wiec zwraca po prostu y :)

x <- list(wiek=c(15,20,22))
y <- list(nazwisko=c("kowalski","polanski","kawka"))
merge2(x,y)    #dziala dla innych nazw niz "a", "b", "c"

x <- list(wiek=c(15,20,22),nazwisko=c("kolski"))
y <- list(nazwisko=c("kowalski","polanski","kawka"),wiek=23)
merge2(x,y)    #wszystko dziala :)

x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=NA)
merge2(x,y)   # NA obslugiwane poprawnie


## ------------------------ Zadanie 03.13 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# funkcja gendyskr() generuje n elementowa pseudolosowa probke z rozkladu 
# dyskretnego zmiennej losowej X okreslonej jako P(X=xi)=pi, gdzie xi i pi 
# wziete sa z wejsciowych wektorow x i p
#
# ARGUMENTY WEJSCIOWE
# 
# n - wartosc calkowita dodatnia
# x - wektor liczbowy o unikalnych wartosciach
# p - wektor liczbowy o dodatnich wartosciach, ktory sumuje sie do jedynki 
#     (w przeciwnym wypadku zostanie umormowany i zostanie wygenerowane 
#     ostrzezenie)
#
# WARTOSC ZWRACANA
#
# n elementowy wektor liczbowy
#

## ---- Funkcja ----

gendyskr <- function(n,x,p){
   
   stopifnot(n>=1)               #czy n jest wieksza rowna 1
   stopifnot(floor(n)==n)        #czy n jest calkowita
   stopifnot(sum(c(is.na(x),is.na(p)))==FALSE)  #czy w x lub p sa NA
   stopifnot(is.numeric(x))      #czy x jest numeryczny
   stopifnot(length(unique(x))==length(x))  #czy x jest wektorem liczbowym  
                                            # o unikalnych wartosciach
   stopifnot(length(x)>=1)       #czy x nie jest pusty
   stopifnot(length(x)==length(p))   #czy x jest tej samej dlugosci, co p
   stopifnot(p<=1 & p>=0)        #czy wartosci p sa miedzy 0 a 1
   
   if(sum(p)!=1){           #spr, czy p sumuje sie do 1, jesli nie to jest 
                            #normowany i generowane jest ostrzezenie
      
      p <- p + (1-sum(p))/length(p)
      warning("Wektor p zostal unormowany!")
   } 
   
   
   f <- function(i){
      u <- runif(1)
      c <- cumsum(p)
      m <- which(c>=u)[1]
      x[m]
   }
   
   u <- unlist(lapply(1:n,f))
   u
   
}

## ---- Przyklady ----

x <- c(5,6,7,2,1,4,8)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)
rle(sort(gendyskr(20,x,p)))  #tu widac, ze wszystko sie zgadza

x <- c(5,-6,7,2,1,4,8)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)    #gdy w x nie ma tylko wartosci naturalnych, tez jest ok

x <- c(5,6,7,2,1,4,8)
p <- c(0.23,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)    #p nie sumuje sie do 1, wiec wyskoczyl warning

x <- c(5,6,7,2,1,4,8)
p <- c(0.2,0.2,0.3,0.11,0.09,5,0.05)
gendyskr(20,x,p)    #error, bo w p jest wartosc wieksza niz 1

x <- c(5,6,7,2,1,4,8,10)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)    #error - x jest dluzszy niz p

x <- c(5,6,7,2,1,4,5)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)    #error - x nie ma unikalnych wartosci

x <- c()
p <- c()
gendyskr(20,x,p)   # error, bo wektory sa puste

x <- c(5,6,NA,2,1,9,8)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)   #error - w x sa NA

x <- c(5,6,777,2,1,9,8)
p <- c(0.2,0.2,NA,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)   #error - w p sa NA

