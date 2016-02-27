## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 02
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 02.05 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# funkcja sort_list() porzadkuje elementy listy x wzgledem wartosci funkcji 
# f wywolanej na kazdej skladowej
#
# ARGUMENTY WEJSCIOWE
#
# x - lista wektorow liczbowych
# f - funkcja agregujaca
#
# WARTOSC ZWRACANA
#
# lista
#

## ---- Funkcja ----

sort_list <- function(x,f){
   
   stopifnot(is.list(x))        
   stopifnot(is.function(f))    
   stopifnot(is.numeric(unlist(x)))   #czy x jest lista wektorow liczbowych
   stopifnot(sum(unlist(lapply(lapply(l,is.na),f)))==0) #jesli w x sa 
                                                        #wartosci NA, to 
                                                        #zwraca blad
   stopifnot(length(f(unlist(x)))<=1)    #spr, czy f jest agregujaca
   
   xa <- lapply(x,f)    #stosuje f do kazdej skladowej listy
   xu <- unlist(xa)     #robi z tego wektor
   xo <- order(xu)      #zapamietuje porzadek
   as.list(x[xo])       #robi posortowana liste
}

## ---- Przyklady ----

l <- list(3,1:4,c(3,8,6,5),1,c(3,3,3,3,3,3))   
sort_list(l,mean)              #dziala :D

l <- list(3,1:4,NA,c(3,NA,8,6,5),1,c(3,3,3,3,3,3))
sort_list(l,mean)      #zwraca blad, bo sa NA

l <- list(3,1:4,c(3,8,6,5),1,c(3,3,3,3,3,3))
sort_list(l,sqrt)    #sqrt nie jest funkcja agregujaca!

l <- list()
sort_list(l,sum)    #blad, bo lista jest pusta


## ------------------------ Zadanie 02.06 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# funkcja remove_el_types() z listy x usuwa elementy o zadanym typie 
# podstawowym
#
# ARGUMENTY WEJSCIOWE
# 
# x - lista 
# type - wektor napisow typow podstawowych
#
# WARTOSC ZWRACANA
# 
# lista
#

## ---- Funkcja ----

remove_el_types <- function(x,type){
   
   stopifnot(is.list(x))               #sprawdza, czy x jest list¹ 
   stopifnot(is.character(type))       #spr, czy type jest wektorem napisow
   
   lap <- lapply(x,typeof)       #dziala na kazda skaldowa listy typeof-em
   nielist <- unlist(lap)        #robi z tego wektor
    
   c <- charmatch(nielist,type,nomatch=-1) #sprawdzam, na ktorych miejscach  
                                            #w wektorze wystepuja te typy 
   a <- which(c>=1)  #ta cala zabawa ponizej jest wlasciwie wynikiem tego, 
                     #ze jesli nie wystepuje ani jeden typ z type w moim 
                     #wektorze x, to wtedy R nie rozumie polecenia 
                     #x[-(i tu wektor pusty)] i dzieki ponizszemu to omijam
   aa <- as.numeric(length(x[-which(c >= 1)])>=1)
   p <- c(aa,!aa)
   w <- which(p==1)
   m <- list(x[-a],x)
   m[[w]]
}

## ---- Przyklady ----

x <- list("adfsfa",75L,c(235,346,"sf"),83475,TRUE,"gr","reyera")
y <- c( "logical","integer")
remove_el_types(x,y)    #dziala!

x <- list("adfsfa",75L,c(235,346,"sf",NA),83475,TRUE,"gr","reyera",NA)
y <- c( "logical","integer")
remove_el_types(x,y)    #tez dziala, bo typeof(NA) to logical

x <- c(2,4,5)
y <- c( "logical","integer")
remove_el_types(x,y)   #error, bo x nie jest lista

x <- list("sg",76,98,"ajhg")
y <- 5
remove_el_types(x,y)   #error, bo y nie jest wektorem wyrazow

x <- list("sg",76,98,"ajhg")
y <- c( "logical","integer", "mamam", NA)
remove_el_types(x,y)   #dziala, nawet, gdy uzytkownik wpisze cos dziwnego


## ------------------------ Zadanie 02.08 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# funkcja factorize() zwraca liste skladajaca sie z dwoch wektorow - 
# pierwszy zawiera wszystkie unikalne elementy z wektora wejsciowego x, 
# a drugi jest wektorem calkowitoliczbowym o dlugosci rownej dlugosci x, 
# ktorego i-tym elementem jest indeks etykiety z pierwszej skladowej rownej 
# x[i]
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow
#
# WARTOSC ZWRACANA
# 
# lista skladajaca sie z dwoch wektorow, z czego pierwszy jest wektorem 
# napisow, a drugi wektorem calkowitoliczbowym o dlugosci rownej dlugosci x
#

## ---- Funkcja ----


factorize <- function(x){
   
   stopifnot(is.character(x))
   
   u <- union(x,x)
   m <- charmatch(x,u)      #wartosciom NA moja funkcja tez przypisuje 
                            #odzielna wartosc
   list(u,m)
}

## ---- Przyklady ----

w <- c("ala","ala","zosia","ala","kasia","ala","zosia","ala","ala","zosia",
       "elwira","bozena","bozena")
wynik <- factorize(w)
wynik
wynik[[1]][wynik[[2]]]     #dziala!


w <- c("ala",NA,"ala","zosia","ala","kasia","ala","zosia","ala","ala","zosia",
       "elwira","bozena","bozena")
wynik <- factorize(w)
wynik
wynik[[1]][wynik[[2]]]    #NA tez dziala poprawnie :D


w <- c("mam", "tak", "134","samo", "mam", "88iiii","tak", "samo", "jestem", 
       "tu", "!", "siabadaba", "234")
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]   #wyrazy skladajace sie nie tylko z liter tez 
                         #segreguje dobrze


w <- c("agr", "33", "agr",33,list(1:9))
factorize(w)   #error, bo w nie jest wektorem napisow

w <- list("sa","ag")
factorize(w)   #to tez jest lista, a nie wektor napisow, wiec error 


## ------------------------ Zadanie 02.09 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# funkcja rnorm2() generuje n obserwacji z rozkladu N(mu,sigma)
#
# ARGUMENTY WEJSCIOWE
# 
# n - liczba calkowita dodatnia
# mu - liczba rzeczywista
# sigma - liczba rzeczywista wieksza od zera
#
# WARTOSC ZWRACANA
# 
# wektor liczbowy o dlugosci n
#

## ---- Funkcja ----

rnorm2 <- function(n,mu=0,sigma=1){
   
   stopifnot(is.double(mu))
   stopifnot(is.double(sigma))
   stopifnot(sigma > 0)
   stopifnot(is.integer(n))
   stopifnot(n>=1)
   
   n2 <- ceiling((n+1)/2)   #w zaleznosci od parzystosci n, 2*n2 bedzie 
                            #wieksze od n o jeden lub o dwa 
   u1 <- runif(n2,0,1)      #losuje z rozkladu jednostajnego n2 liczb
   u2 <- runif(n2,0,1)
   
   lo <- sqrt(-2*log(u1))   #wartosci pomocnicze
   po <- 2*pi*u2
   
   z1 <- lo*cos(po)   
   z2 <- lo*sin(po)
   zz <- c(z1,z2)    #ten wektor jest wiecej niz n elementowy
   lz <- length(zz)   
   c <- ceiling(runif(lz-n,0,lz))  #losuje tyle wartosci, ile mam za duzo 
                                   #i nizej je usuwam
   zzz <- zz[-c]
   
   z <- mu +sigma*zzz
   z
}

## ---- Przyklady ----

rnorm2(100L,0,1)  #dziala :D
rnorm2(100L)      #ok
rnorm2(101L)      #ok 
rnorm2(23L,3)     #dziala
rnorm2(-5L)        #error, bo n <= 0
rnorm2(-4.2,1,0.9)  #error
rnorm2(3L,1,-0.9)   #error


## ------------------------ Zadanie 02.10 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# funkcja approxinvert() wyznacza przyblizenie odwrotnosci zwektoryzowanej,
# ciaglej i scisle monotonicznej na pewnym przedziale [a,b] funkcji f 
# w punktach z wektora y za pomoca interpolacji f w k rownoodleglych 
# punktach z przedzialu [a,b]
#
# ARGUMENTY WEJSCIOWE
#
# f - zwektoryzowana,ciagla, scisle monotoniczna na przedziale [a,b] funkcja
# y - wektor liczbowy o elementach z [f(a),f(b)]
# a - wartosc rzeczywista
# b - wartosc rzeczywista taka, ze b>a
# k - wartosc calkowita dodatnia, k>2 
#
# WARTOSC ZWRACANA
#
# wektor liczbowy
#

## ---- Funkcja ----

approxinvert <- function(f,y,a,b,k=100L){
   
   stopifnot(y>=f(a) & y<=f(b))
   stopifnot(is.double(a))
   stopifnot(is.double(b))
   stopifnot(length(a)==1 & length(b)==1)
   stopifnot(a<b)
   stopifnot(k>2 & is.integer(k))
   stopifnot(is.numeric(y))
   stopifnot(length(f(y))==length(y)) #sprawdza, czy f jest zwektoryzowana 
                                                            
   kk <- seq(a,b,length.out=k)
   a <- approxfun(f(kk),kk)
   a(y)
}

## ---- Przyklady ----

f <- function(x){
   x+2
} 
y <- c(-2,NA,4,4.44)
approxinvert(f,y,-4,5)   #jesli uzytkownik uzyje NA, to f zwraca blad
approxinvert(f,1,-4,5)
approxinvert(sum,y,-4,-5,100L)  #error


## ------------------------ Zadanie 02.11 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# funkcja chrapaniesynchroniczne() okresla, czy w trakcie 90-minutowego 
# wykladu darzy sie sytuacja, ze wszyscy studenci zachrapia 
# jednoczesnie, jesli kazdy chrapie przez jedna minute w stalych odstepach  
# czasu danych za pomoca wektora x, i kazdy student zasypia po y minutach
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor liczbowy calkowity dodatni
# y - wektor liczbowy calkowity dodatni
#
# WARTOSC ZWRACANA
# 
# wartosc logiczna TRUE lub FALSE
#

## ---- Funkcja ----

chrapaniesynchroniczne <- function(x,y){
   
   stopifnot(is.na(x)==FALSE)
   stopifnot(is.na(y)==FALSE)
   stopifnot(is.integer(x) & x>=0)
   stopifnot(is.integer(y) & y>=0)
   stopifnot(length(x)==length(y))
   
   n <- length(x)    #tylu jest studentow
   i <- 1:n
   
   nn <- rep(1:90,each=n)
   xx <- numeric(90)
   xx[1:n] <- x
   
   a <- y[i] + xx[i]*nn
   
   aa <- 1:(n*90)
   aaa <- aa%%n
   aaa[which(aaa==0)] <- n
   
   aaaa <- rep(aaa,n)
   bbbb <- rep(1:n,each=n*90)
   
   z <- which(aaaa==bbbb)
   zz <- z%%(n*90)
   zz[which(zz==0)] <- n*90
   
   mm <- a[zz]
   m <- mm[-which(mm>90)]
   
   s <- sort(m)
   r <- rle(s)
   w <- which(r$lengths==n)
   
   as.logical(length(w))
}

## ---- Przyklady ----

x <- as.integer(c(6,3,10,3,90,444))
y <- as.integer(c(0,15,30,0,0,99))
chrapaniesynchroniczne(x,y)

x <- as.integer(c(6,3,10))
y <- as.integer(c(0,15,30))
chrapaniesynchroniczne(x,y)

x <- c(6,3,10.4)
y <- as.integer(c(0,15,30))
chrapaniesynchroniczne(x,y)  #error

x <- c(6,3,NA)
y <- as.integer(c(0,15,30))
chrapaniesynchroniczne(x,y)  #error

x <- as.integer(c(6,3,10,90,444))
y <- as.integer(c(0,15,30,0,0,99))
chrapaniesynchroniczne(x,y)    #error

