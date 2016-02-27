# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 04
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 04.09 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja inkwizycja() sprawdza, czy graf skierowany (dany w postaci 
# macierzy zero-jedynkowej) jest niemoralny (gdy jest niemoralny zwraca 
# TRUE, a gdy nie jest - FALSE). Graf niemoralny to taki, w ktorym istnieja
# dwa wierzcholki posiadajace wspolnego potomka, ktore nie sa polaczone 
# zadna krawedzia. 
# Gdy w macierzy pojawiaja sie NA, to funkcja wstawia na ich miejsca 0 (
# czyli tak, jakby nie bylo tam krawedzi).
#
# ARGUMENTY WEJSCIOWE
# 
# m - kwadratowa macierz 0-1  
#
# WARTOSC ZWRACANA
# 
# jednoelementowy wektor logiczy (krotko mowiac TRUE albo FALSE)
#


## ---- Funkcja ----

inkwizycja <- function(m){
   
   stopifnot(is.matrix(m))   #czy m jest macierza
   stopifnot(is.numeric(m))  #czy m jest macierza numeryczna
   
   dd <- dim(m)
   
   stopifnot(dd[1]==dd[2])   #czy m jest macierza kwadratowa
   
   m[is.na(m)] <- 0    #gdy w macierzy pojawia sie NA, to traktuje je tak, 
                       #jakby nie bylo tam krawedzi
   
   stopifnot(is.element(as.vector(m),c(0,1)))  #czy m jest macierza 0-1
   
   d <- dd[1]
   
   m[diag(m)] <- 0     #zeby nie plataly sie niepotrzebne petle, ktorych i 
                       #tak nie bierzemy pod uwage w rozwazaniach
   
   for(j in 1:d){        #przechodzimy po kolumnach macierzy m
      if(sum(m[,j])>1){  #jesli w danej kolumnie beda co najmniej dwie 
                         #jedynki, oznacza to, ze j jest potomkiem co 
                         #najmniej dwoch innych wierzcholkow
         w <- which(m[,j]==1)    #sprawdzam, ktorych
         
         if(all(as.vector(m[w,w])==0) & all(as.vector(m[j,w])==0)){
                                 #pierwszy warunek sprawdza, czy nie ma
                                 #zadnej krawedzi miedzy wierzcholkami z w,
                                 #bo jesli jest, to wszystko jest w porzadku
                                 #drugi warunek sprawdza, czy potomek jest 
                                 #potomkiem (bo jesli od niego bedzie 
                                 #odchodzila krawedz powrotna, to nim nie 
                                 #bedzie)
            return(TRUE)   #jesli powyzsze jest spelnione, to nie musimy 
                           #juz sprawdzac kolejnych wierzcholkow, bo jeden
                           #juz na pewno istnieje i mozemy zwrocic TRUE
         } 
      }
   }
   
   FALSE     #w przeciwnym przypadku zwracamy FALSE
   
}

## ---- Przyklady ----

m <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0), ncol=4)
inkwizycja(m)  #dziala :)

m <- matrix(c(0,0,0,0,1,1,0,0,1,0,0,1,0,0,0,0), ncol=4)
inkwizycja(m)  #dziala tez, gdy na przekatnej jest jedynka :)

m <- matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,0),ncol=5)
inkwizycja(m)     #dziala

m <- matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,1,0),ncol=5)
inkwizycja(m)     #dziala

m <- matrix(c(0,0,0,2,1,0,0,0,1,0,0,1,0,0,0,0), ncol=4)
inkwizycja(m)   #error - nie jest zero-jedynkowa

inkwizycja(matrix())   #error dla macierzy pustej

m <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0), ncol=4)
inkwizycja(m)   #ok

m <- matrix(c("hfhhf",0,0,0,1,0,0,0,1,0,0,1,0,0,1,0), ncol=4)
inkwizycja(m)   #error - nie jest numeryczna

m <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,1,1,1), ncol=4)
inkwizycja(m)   #error - nie jest kwadratowa

m <- matrix(c(0,0,NA,0,1,0,0,0,1,0,0,1,0,0,1,0), ncol=4)
inkwizycja(m)   #dla NA tez dziala :)



## ------------------------ Zadanie 04.10 ----------------------------

## ---- Dokumentacja ----

# OPIS
#
# Funkcja permutacje() wypisuje wszystkie mozliwe permutacje n-elementowego
# wektora atomowego o elementach unikalnych. 
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor atomowy o elementach unikalnych
#
# WARTOSC ZWRACANA
# 
# macierz o n! wierszach i n kolumnach (gdzie n to dlugosc wektora x)
#

## ---- Funkcja ----

permutacje <- function(x){
   
   stopifnot(is.atomic(x))   #czy x jest wektorem atomowym 
   
   n <- length(x)
   
   stopifnot(n>=1)           #dlugosc x musi byc co najmniej 1
   
   if(n==1) return(x)
   
   stopifnot(length(unique(x))==n)   #czy w x sa elementy unikalne
   
   tab <- matrix(0,nrow=factorial(n),ncol=n)  #tworze macierz, w ktorej beda
                                              #zapisywane permutacje
   p <- sort(x)
   tab[1,] <- p   #pierwszy rzad macierzy staje sie pierwsza permutacja
                  #posortowana alfabetycznie
   
   for(z in 2:factorial(n)){    #przechodze petla po wszystkich wierszach
                                #wyjsciowej macierzy
      
      for(i in n:2){            #sprawdzam, przechodzac po wektorze od tylu,
                                #czy jest posortowany rosnaco i znajduje 
                                #pierwszy element, ktory psuje szyk
         if(p[i-1] < p[i]) break
      }                       #i bedzie wiec teraz indeksem poprzedzajacego 
                              #go elementu 
      
      for(k in n:i){       #przechodze petla po wszystkich koncowych  
                           #elementach wektora (tych posortowanych rosnaco) 
                           #i szukam pierwszego wiekszego od mojego elementu
                           #na miejscu (i+1) i je zamieniam
         if(p[k]>p[i-1]){
            a <- p[i-1]
            p[i-1] <- p[k]
            p[k] <- a
            break
         }
      }
      
      p[i:n] <- sort(p[i:n])   #sortuje te koncowke z zamieniona wartoscia
      tab[z,] <- p             #i wpisuje te permutacje na kolejne miejsce 
                               #tablicy (algorytmem tym otrzymam wszystkie 
                               #permutacje uporzadkowane leksykograficznie)
   }
   
   tab
   
}

## ---- Przyklady ----

permutacje(1:3)  #na liczbach - ok

permutacje(c("ss","hg","ag"))  #na literkach tez :)

permutacje(c("dff",4,TRUE))    #na wektorze mieszanym tez dziala :)

permutacje(1:6)    #na dosc dlugim tez ok :D

permutacje(c())    #error - za krotki wektor

permutacje(8)      #dla wektora jednoelementowego - ok

permutacje(c(8,8))  # error - x nie ma unikalnych wartosci 


## ------------------------ Zadanie 04.11 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja plansza_sudoku() sprawdza, czy dana macierz kwadratowa jest 
# poprawna macierza do gry w k-sudoku. Zwraca odpowiednio TRUE lub FALSE.
# Co znaczy, ze macierz jest poprawnym k-sudoku?
# - ma k^2 wierszy i k^2 kolumn (czyli dodatkowo jest kwadratowa)
# - zawiera tylko elementy ze zbioru {1,...,k^2} oraz wartosci NA
# - w kazdym wierszu, kazdej kolumnie i kazdym rozlacznym podkradracie o 
#   o boku k*k wartosci liczbowe nie moga sie powtarzac
#
# ARGUMENTY WEJSCIOWE
# 
# m - macierz kwadratowa o elementach calkowitych
#
# WARTOSC ZWRACANA
# 
# TRUE lub FALSE (jednoelementowy wektor logiczny)
#

## ---- Funkcja ----

plansza_sudoku <- function(m){
   
   stopifnot(is.matrix(m))    #czy m jest macierza
   stopifnot(is.numeric(m))   #czy ma jest macierza numeryczna
   
   d <- dim(m)
   
   stopifnot(d[1]==d[2])  #czy m jest macierza kwadratowa   
   
   stopifnot(length(which(as.vector(m)==0))==0)  #sprawdza, czy w macierzy
                                                 #sa zera, jesli sa to 
                                                 #zwraca blad, a jesli nie 
                                                 #ma, to smialo moge zrobic
                                                 #nastepny krok
   m[is.na(m)] <- 0   #dla wygody wszystkie NA z macierzy zamieniam na zera
   
   k <- sqrt(d[1])    #k to jest pierwiastek wymiaru
   
   stopifnot(floor(k)==k)  #sprawdzam, czy k jest liczba calkowita
   stopifnot(all(as.vector(floor(m)==m)==TRUE))  #sprawdzam, czy m jest 
                                                 #macierza calkowitoliczbowa
   
   if(!all(m<=d[1])) return(FALSE) #sprawdzam, czy elementy w m sa mniejsze
                                   #niz wymiar m, jesli nie - funkcja zwraca
                                   #FALSE
   if(!all(m>=0)) return(FALSE)    #analogicznie, czy sa wieksze rowne niz 
                                   #zero (rowne, bo sama sobie przed chwila
                                   #te zera dolozylam)
   
   v <- numeric(3*d[1])  #wektor pomocniczy 
   
   for(i in 1:d[1]){      #petla po wszystkich wierszach
      l1 <- length(which(m[i,]==0))  #ile jest elementow, ktore uzytkownik
                                     #ma sam wypelnic jest w i-tym wierszu
      if(l1==0) l1 <- 1     #jesli nie ma zadnego, to l1 staje sie jedynka -
                            #widac w nastepnej linijce, po co
      v[i] <- length(unique(m[i,]))==d[1]-(l1-1) #na k^2 pierwszch miejsc
                                                 #wstawiamy TRUE albo FALSE
                                                 #zaleznie od tego, czy w 
                                                 #i-tym wierszu sa unikalne
                                                 #elementy
      
      l2 <- length(which(m[,i]==0))   #analogicznie dla i-tej kolumny (tyko
                                      #na k^2 kolejnych miejsc w v)
      if(l2==0) l2 <- 1
      v[d[1]+i] <- length(unique(m[,i]))==d[1]-(l2-1)
   }
   
   z <- matrix(0,ncol=k,nrow=k)   #macierz pomocnicza k*k
   s <- 0                         #pomocnicza wartosc
   
   for(i in 0:(k-1)){      
      for(r in 0:(k-1)){
         z <- m[(i*k+1):(i*k+k),(r*k+1):(r*k+k)]   #wybieramy odpowiedni 
                                                   #maly kwadrat 
         a <- as.vector(z)
         ll <- length(which(a==0)) 
         if(ll==0) ll <- 1
         
         v[2*d[1]+r+1+k*s] <- length(unique(a))==d[1]-ll+1  #i sprawdzamy, 
                                                            #czy sa w nim
                                                            #unikalne 
                                                            #elementy
      }
      s <- s+1
   }
   
   if(all(as.logical(v))==TRUE) TRUE else FALSE #jesli wszystkie wartosci w 
                                                #v sa prawdziwe, to to jest
                                                #poprawna plansza, jesli nie
                                                #zgadza sie choc jedno 
                                                #miejsce, to na pewno nie 
                                                #jest
   
}

## ---- Przyklady ----

m <- matrix(c(7,4,8,9,6,1,3,5,2,2,6,5,4,8,3,9,7,1,9,3,1,5,7,2,6,4,8,6,9,3,
              1,4,5,8,2,7,1,7,4,6,2,8,5,3,9,5,8,2,7,3,9,4,1,6,3,1,6,2,9,4,
              7,8,5,8,2,7,3,5,6,1,9,4,4,5,9,8,1,7,2,6,3), ncol=9, 
            byrow=TRUE)
plansza_sudoku(m)   # ok - dla calej wypelnionej

m <- matrix(c(7,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
              9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
              1,6,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
            ncol=9, byrow=TRUE)
plansza_sudoku(m)   # ok - dla takiej z wartosciami NA

plansza_sudoku(matrix())   #error - dla pustej macierzy

m <- matrix(c(7,4,2,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
              9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
              1,6,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
             ncol=9, byrow=TRUE)
plansza_sudoku(m)   # ok - to nie jest plansza sudoku

m <- matrix(c(3,NA,NA,NA,NA,2,NA,1,NA,4,NA,NA,NA,NA,NA,2),ncol=4)
plansza_sudoku(m)  #zgadza sie dla 2-sudoku

m <- matrix(c(3,NA,NA,NA,NA,2,NA,1,NA,4,2,NA,NA,NA,NA,2),ncol=4)
plansza_sudoku(m)  #zdadza sie :)

m <- matrix(c(7.5,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
              9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
              1,6,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
            ncol=9, byrow=TRUE)
plansza_sudoku(m)  #error - wartosci nie sa calkowite

m <- matrix(c(7,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
              9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
              1,16,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
            ncol=9, byrow=TRUE)
plansza_sudoku(m)  #FALSE - nie wszystkie elementy mniejsze od 9

m <- matrix(c(7,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
              9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
              1,-1,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
            ncol=9, byrow=TRUE)
plansza_sudoku(m) #FALSE - nie wszystkie elementy wieksze od 1


## ------------------------ Zadanie 04.12 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja naiwne_sudoku() dla poprawnej planszy do gry w k-sudoku (tego 
# warunku nie sprawdzam - uzytkownik musi wpisac sam dobrze - bez sensu 
# bowiem wydaje mi sie przekopiowywanie poprzedniego zadania) wstawi zamiast
# NA wartosci liczbowe, ktore z pewnoscia nalezy wstawic w te pola. Wynikiem
# dzialania bedzie macierz, na ktorej nie da sie wstwic zadnej kolejnej 
# wartosci bez zgadywania. 
#
# ARGUMENTY WEJSCIOWE
# 
# m - macierz kwadratowa o wartosciach calkowitoliczbowych
#
# WARTOSC ZWRACANA
# 
# macierz tych samych wymiarow, co m, w ktorej zamiast niektorych wartosci 
# NA wstawione sa liczby naturalne
#

## ---- Funkcja ----

naiwne_sudoku <- function(m){
   
   stopifnot(is.matrix(m))    #czy m jest macierza
   stopifnot(is.numeric(m))   #czy ma jest macierza numeryczna
   
   d <- dim(m)
   
   stopifnot(d[1]==d[2])  #czy m jest macierza kwadratowa   
   
   stopifnot(length(which(as.vector(m)==0))==0)  #sprawdza, czy w macierzy
                                                 #sa zera, jesli sa to 
                                                 #zwraca blad, a jesli nie 
                                                 #ma, to smialo moge zrobic
                                                 #nastepny krok
   m[is.na(m)] <- 0   #dla wygody wszystkie NA z macierzy zamieniam na zera
   
   n <- d[1]
   nn <- sqrt(n)
   e <- 1:n
   
   stopifnot(floor(nn)==nn)  #sprawdzam, czy k jest liczba calkowita
   stopifnot(all(as.vector(floor(m)==m)==TRUE))#sprawdzam, czy m jest 
                                                 #macierza calkowitoliczbowa
   
   #zakladam, ze reszte warunkow na bycie k-sudoku, uzytkownik wpisal sam 
   #dobrze (sprawdzilam tylko te niezbedne)
   
   f <- function(i){                  #pomocnicza funkcja zwracajaca 
                                      #przeciecie elementow list s,k i z
      a <- intersect(s[[i]],k[[i]])
      intersect(a,z[[i]])
   }
   
   ff <- function(i){            #pomocnicza funkcja sprawdzajaca, czy i-ty
                                 #element listy wsp jest jednoelementowy
      length(wsp[[i]])==1
   }
   
   repeat{  
      
      k <- vector("list",n*n)  #tworze trzy pomocnicze listy, na ktorych 
                               #odpowiednio bede zapisywac, ktore elementy
                               #w danym pustym miejscu macierzy moge wpisac 
                               #(ze wzgledu na wartosci w danej kolumnie,
                               #wierszu i kwadraciku)
      s <- vector("list",n*n)
      z <- vector("list",n*n)
      
      for(j in 1:n){  #przechodze po kolumnach i potem po wierszach macierzy
         for(i in 1:n){
            if(m[i,j]==0){ #jesli element macierzy jest pusty, to wtedy:
               
               if(all(m[i,]==0)){  
                  k[[n*(j-1)+i]] <- e 
               } else{
                  k[[n*(j-1)+i]] <- e[-m[i,]] #na odpowiednie miejsce listy
                                              #k wpisuje liczby ze zbioru 
                                              #{1,...,n} dla i-tego 
                                              #wiersza, ktore sie jeszcze
                                              #w tym wierszu nie pojawily 
                                              #(czyli tak jakby wszystkie 
                                              #potencjalne do wpisania)
                                              #ten if jest po to, ze psulo
                                              #sie, gdy odejmowalam z e 
                                              #pusty wektor
               }
               
               if(all(m[,j]==0)){       #analogicznie dla kolumn i listy s
                  s[[n*(j-1)+i]] <- e 
               } else{
                  s[[n*(j-1)+i]] <- e[-m[,j]]
               }
               
               d <- (i-1) %/% nn   #analogicznie dla kwadracikow i listy z
               dd <- (j-1) %/% nn
               
               if(all(as.vector(m[(d*nn+1):(d*nn+nn),
                                  (dd*nn+1):(dd*nn+nn)])==0)){
                  z[[n*(j-1)+i]] <- e
               } else{
                  z[[n*(j-1)+i]] <- e[-as.vector(m[(d*nn+1):(d*nn+nn),
                                                   (dd*nn+1):(dd*nn+nn)])]
               }            
            } 
         }
      }
      
      wsp <- lapply(1:(n*n),f)  #rozwazam przeciecie list k,s i z
      
      if(!any(unlist(lapply(1:(n*n),ff)))) break #jesli choc jedno 
                                                 #przeciecie nie jest
                                                 #jednoelementowe, to 
                                                 #konczymy petle (bo trzeba
                                                 #by juz zgadywac)
      
      for(i in 1:length(wsp)){   
         if(length(wsp[[i]])==1) m[i] <- wsp[[i]] #tam, gdzie przeciecie 
                                                  #jest jednoelementowe, 
                                                  #wstawiamy odpowiednia 
                                                  #wartosc
      }
      
   }
   
   m[which(as.vector(m)==0)] <- NA   #tam, gdzie na poczatku wstawilam dla
                                     #wygody zera, teraz (jesli jeszcze 
                                     #takie sa) zwracam NA
   m
   
}

## ---- Przyklady ----

m <- matrix(c(3,NA,NA,NA,NA,2,NA,1,NA,4,NA,NA,NA,NA,NA,2),ncol=4)
naiwne_sudoku(m)   #dziala

m <- matrix(c(NA,NA,NA,NA,5,3,NA,NA,NA,9,NA,NA,1,6,8,2,NA,NA,4,NA,NA,NA,NA,
              NA,8,1,5,6,NA,2,NA,NA,NA,3,NA,NA,NA,8,4,NA,9,6,NA,2,NA,NA,NA,
              NA,NA,1,5,NA,NA,8,8,NA,NA,NA,NA,NA,1,9,3,2,9,NA,NA,NA,NA,NA,
              NA,7,7,3,6,NA,NA,NA,5,NA,NA),ncol=9)
naiwne_sudoku(m)   #dziala

m <- matrix(c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,3,NA,NA,NA),ncol=4)
naiwne_sudoku(m)   #ok - wstawilo tam, gdzie sie da, a reszte trzeba by 
                   #     zgadywac

m <- matrix(c(1.4,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,NA,3,NA,NA,NA),ncol=4)
naiwne_sudoku(m)  #error - wartosci nie sa calkowita

naiwne_sudoku(matrix())  #error - macierz pusta


## ------------------------ Zadanie 04.14 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkacia domk_przech() wyznacza domkniecie przechodnie relacji R danej
# za pomoca kwadratowej macierzy zero-jedynkowej. Domkniecie przechodnie 
# relacji R, to taka relacja R', ze R jest podzbiorem R' oraz R' jest  
# przechodnia. Jesli w macierzy wejsciowej pojawia sie NA, to zostana 
# zamienione na 0 (czyli tak, jakby nie bylo tam relacji).
#
# ARGUMENTY WEJSCIOWE
# 
# m - kwadratowa macierz 0-1
#
# WARTOSC ZWRACANA
#
# kwadratowa macierz 0-1 
#

## ---- Funkcja ----

domk_przech <- function(m){
   
   stopifnot(is.matrix(m))  #czy m jest macierza
   stopifnot(is.numeric(m))  #czy m jest macierza numeryczna
   
   dd <- dim(m)
   
   stopifnot(dd[1]==dd[2])  #czy m jest kwadratowa
   
   m[is.na(m)] <- 0
   
   stopifnot(is.element(as.vector(m),c(0,1)))  #czy m jest macierza 0-1
   
   n <- dd[1]
   mm <- m    # macierz pomocnicza i nastepnie wyjsciowa
   
   for(i in 1:n){              #przechodze po wierszach
      for(j in 1:n){           #i kolumnach
         if(m[i,j]==1){        #jesli i jest w relacji z j
            w <- which(m[j,]==1)   #to sprawdzam, z ktorymi j jest w relacji
            if(length(w)==0) next  #jesli z zadnymi to dalej
            mm[i,w] <- 1           #jesli z jakimis, to i tez musi byc z 
                                   #nimi w relacji
         }
      }
   }
   
   mm
   
}

## ---- Przyklady ----

m <- matrix(c(0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1),ncol=4)
domk_przech(m)   #ok

m <- matrix(c(0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)   #ok

m <- matrix(c(0,4,1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)   #error - macierz nie jest 0-1

m <- matrix(c(0,"k",1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)   #error - macierz nie jest numeryczna

m <- matrix(c(0,1,1,0,1,0,1,0,0,0,0,0),ncol=4)
domk_przech(m)   #error - macierz nie jest kwadratowa

m <- matrix(c(1,0,1,0,1,0,0,1,0),ncol=3)
domk_przech(m)   #ok :)

m <- matrix(c(1,1,1,1,1,1,1,1,1),ncol=3)
domk_przech(m)   #dla grafu pelnego tez sie zgadza :)

m <- matrix(c(0,1,1,1,0,1,1,1,0),ncol=3)
domk_przech(m)   #ok 

m <- matrix(c(0,NA,1,1,0,1,1,1,0),ncol=3)
domk_przech(m)   #ok - NA zamienia na 0 


## ------------------------ Zadanie 04.15 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja klabs() sprawdza, czy relacja dana w postaci macierzy 0-1, jest 
# relacja rownowaznosci (czy jest zwrotna, symetryczna i przechodnia). Jesli
# nie jest - zwraca blad. Jesli jest funkcja znajduje wszystkie klasy 
# abstrakcji tej relacji i zwraca je w postaci listy (i-ty element listy,
# to elementy, ktore tworza klase abstrakcji elementu i). Jesli w macierzy 
# wejsciowej pojawia sie NA, to zostana zamienione na 0 (czyli tak, jakby 
# nie bylo tam relacji).
#
# ARGUMENTY WEJSCIOWE
# 
# m - kwadratowa macierz 0-1
#
# WARTOSC ZWRACANA
#
# lista calkowitoliczbowa o n-elementach, gdzie n to wymirar macierzy m
#

## ---- Funkcja ----

klabs <- function(m){
   
   stopifnot(is.matrix(m))  #czy m jest macierza
   stopifnot(is.numeric(m))  #czy m jest macierza numeryczna
   
   dd <- dim(m)
   
   stopifnot(dd[1]==dd[2])  #czy m jest kwadratowa
   
   m[is.na(m)] <- 0
   
   stopifnot(is.element(as.vector(m),c(0,1)))  #czy m jest macierza 0-1
   
   if(!all((diag(m)==1))) stop("Relacja nie jest zwrotna!")         
   if(any(as.vector(t(m)==m)==FALSE)) stop("Relacja nie jest symetryczna!")            
   
   n <- dd[1]
   
   for(i in 1:n){    #przechodze po wszystkich elementach, zeby spr, czy 
                     #relacja jest przechodnia
      for(j in 1:n){
         if(m[i,j]==1){          #jesli i jest w relacji z j
            w <- which(m[j,]==1) #to sprawdzam, z ktorymi j jest w relacji
            if(length(w)==0) next   #jesli z zadnymi to dalej
            if(any(as.vector(m[i,w])!=1)) #jesli z jakimis, to i tez musi  
                                          #byc z nimi w relacji, wiec jesli 
                                          #nie jest -> blad
               stop("Relacja nie jest przechodnia!")
         }
      }
   }
   
   ka <- vector("list",n)    #tworze liste, na ktorej bede umieszczac klasy
                             #abstrakcji
   
   for(i in 1:n){            #przechodze po kolejnych wierszach
      w <- which(m[i,]==1)   #i sprawdzam, z ktorymi elementami i jest w 
                             #relacji
      ka[[i]] <- w           #te, z ktorymi jest, wpisuje na kolejne miejsce
                             #na mojej liscie
   }
   
   ka
   
}

## ---- Przyklady ----

m <- matrix(c(0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1),ncol=4)
klabs(m)  #error

m <- matrix(c(1,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1),ncol=4)
klabs(m)  #error

m <- matrix(c(1,0,0,1,0,1,0,1,0,0,1,0,1,1,0,1),ncol=4)
klabs(m)  #error

m <- matrix(c(1,0,0,0,0,1,1,1,0,1,1,1,0,1,1,1),ncol=4)
klabs(m)  #ok

m <- matrix(c(1,1,1,1,1,1,1,1,1),ncol=3)
klabs(m)  #ok

m <- matrix(c(1,0,1,0,0,1,0,0,0,0,1,1,0,0,1,1),ncol=4)
klabs(m)  #error

klabs(matrix())   #error dla macierzy pustej

klabs(diag(4))    #ok

m <- matrix(c(1,NA,1,1,1,1,1,1,1),ncol=3)
klabs(m)  #obsluga NA - ok

m <- matrix(c(1,4,1,1,1,1,1,1,1),ncol=3)
klabs(m)  #error - m nie jest macierza 0-1


## ------------------------ Zadanie 04.16 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja relrown() dla danej listy wektorow calkowitoliczbowych zwraca 
# kwadratowa macierz 0-1 reprezentujaca relacje rownowaznosci generowana
# przez dane klasy abstrakcji.
#
# ARGUMENTY WEJSCIOWE
# 
# l - lista calkowitoliczbowa
#
# WARTOSC ZWRACANA
# 
# kwadratowa macierz 0-1
#

## ---- Funkcja ----

relrown <- function(l){
   
   stopifnot(is.list(l))   #czy l jest liczba
   
   u <- unlist(l)
   
   stopifnot(is.numeric(u))   #czy l jest lista numeryczna
   stopifnot(floor(u)==u)     #czy jest calkowitoliczbowa
   
   ll <- length(l)
   
   stopifnot(max(u)<=ll)  #najwiekszy element mniejszy niz dlugosc
                                 #listy
   stopifnot(min(u)>=1)   #najmniejszy element wiekszy niz 1
      
   m <- matrix(0,ncol=ll,nrow=ll)  #macierz pomocnicza 
   
   for(i in 1:ll){
      m[i,l[[i]]] <- 1   #wpisuje 1 do macierzy, jesli jest tam relacja
   }
   
   m
   
}


## ---- Przyklady ----

l <- list(1,2:4,2:4,2:4)
relrown(l)  #ok

l <- list(1,2,3:4,3:4)
relrown(l)  #ok

l <- list(1,2,3,4)
relrown(l)  #ok

l <- list()
relrown(l)  #error - lista pusta

l <- list(6,8,2)
relrown(l)  #error 

l <- list(1,2.2)
relrown(l)  #error

l <- list("fff",1)
relrown(l)  #error


