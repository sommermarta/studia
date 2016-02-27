## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 01
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



## ------------------------ Zadanie 01.05 ----------------------------

## ---- Dane wejsciowe ----

x <- round(runif(10,-5,10))    #wybieram losowy wektor x

## ---- Operacje ----

y <- sort(x)            #sortuje go rosnaco                        
l <- rle(y)$lengths     #ile razy wystepuja kolejne liczby
v <- rle(y)$values
s <- which(l==max(l))   #nr indeksu liczby, ktora powtarza sie najczesciej
t <- v[s[1]]            #jesli rozwiazanie nie jest jednoznaczne, 
                        #to wyswietla dowolna (u mnie - najmniejsza) mode

## ---- Prezentacja wyników ----

x       #dany wektor
y       #ten wektor posortowany
t       #moda



## ------------------------ Zadanie 01.07 ----------------------------

## ---- Dane wejsciowe ----

x <- c("a","b","c","d","e","f","g")   #przykladowy wektor atomowy
k <- round(runif(1,1,25))             #losowa liczba nieujemna k

## ---- Operacje ----

y <- round(runif(k,1,length(x)))     #wektor dlugosci k liczb calkowitych 
                                     #ze zbioru {1,...,length(x)} 
                                     #(naszych pozniejszych indeksow)
los <- x[y]

## ---- Prezentacja wyników ----

k            #ile mialo byc liczb
los          #szukany wektor losowy
 


## ------------------------ Zadanie 01.08 ----------------------------

## ---- Dane wejsciowe ----

x <- c(TRUE,FALSE,FALSE,FALSE,NA,TRUE,FALSE,NA,TRUE,TRUE,FALSE,TRUE)
beta <- 0.95

## ---- Operacje ----

y <- na.omit(x)    #usowam wartosci NA
n <- length(y)     #dlugosc nowego wektora
alfa <- 1-beta     
a1 <- alfa/2
a2 <- 1-alfa/2

kk <- which(y==TRUE)  #wektor indeksow, gdzie jest wartosc TRUE
k <- length(kk)       #liczba wartosci TRUE

q <- qnorm(a2, 0, 1)    #wartosc funkcji kwantylowej rozkladu normalnego
p <- sqrt(k*(n-k)/(n^3))  #ten duzy pierwiastek :D
al <- k/n-q*p           #lewy kraniec przedzialu asymptotycznego
ap <- k/n+q*p           #prawy kraniec przedzialu asymptotycznego

bl <- qbeta(a1 , k , n-k+1)  #lewy kraniec przedzialu Cloppera-Pearsona
bp <- qbeta(a2 , k+1 , n-k)  #prawy kraniec przedzialu Cloppera-Pearsona

da <- ap-al   #dlugosc przedzialu asymptotycznego
db <- bp-bl   #dlugosc przedzialu Cloppera-Pearsona

## ---- Prezentacja wyników ----

da    #dlugosc przedzialu asymptotycznego  
db    #dlugosc przedzialu Cloppera-Pearsona



## ------------------------ Zadanie 01.09 ----------------------------

## ---- Dane wejsciowe ----

x <- c(2,3,4,2,2,4,4,3)  #dwa dane wektory liczbowe
y <- c(3,5,6,5,4,3,2,2)

## ---- Operacje ----

n <- length(x)     #dlugosc wektora x (i y tez, bo mialy taka sama dlugosc)
rx<-rank(x)        #ranga obserwacji z wektora x
ry<-rank(y)        #ranga obserwacji z wektora y
d<-rx-ry           #roznica miedzy rangami (d ze wzoru)

ro <- 1 - (6*sum(d^2))/(n*(n^2-1))   #wartosc probkowego estymatora 
                                     #wspolczynnika korelacji rangowej 
                                     #Spearmana
t <- 1 - pt(ro*sqrt((n-2)/(1-ro^2)),n-2)  #p-wartosc testu istotnosci tego 
                                          #wspolczynnika korelacji

## ---- Prezentacja wyników ----

t     #szukana p-wartosc



## ------------------------ Zadanie 01.11 ----------------------------

## ---- Dane wejsciowe ----

x <- round(runif(13,1,10)) #dane wektory o elementach calkowitych dodatnich
y <- round(runif(10,1,10))

## ---- Operacje ----

tx <- tabulate(x)   #ile jest w wektorze x kolejno liczb 1,...,max(x)
ty <- tabulate(y)   #ile jest w wektorze y kolejno liczb 1,...,max(y)
nx <- length(tx)  #dlugosc wektora tx
ny <- length(ty)  #dlugosc wektora ty
m <- min(nx,ny)   #minimum dlugosci wektora tx i ty
tx2 <- tabulate(x,nbins=m)    #ile jest w wektorze tx kolejno liczb 1,...,m
ty2 <- tabulate(y,nbins=m)    #ile jest w wektorze ty kolejno liczb 1,...,m
                              
b <- (which(tx2!=0 & ty2!=0)) #dzieki stworzeniu tx2 i tx1 nie ma problemu 
                              #z regula zawijania; a i tak do czesci 
                              #wspolnej nie beda nalezaly liczby, ktore nie 
                              #naleza do jednego z wektorow

## ---- Prezentacja wyników ----

x; y     #wektory x i y
b        #czesc wspolna stworzona przeze mnie
intersect(x,y)   #czesc wspolna przy uzyciu funkcji intersect()



## ------------------------ Zadanie 01.12 ----------------------------

## ---- Dane wejsciowe ----

x <- c(1,5,6,9,7,3,5,6,4)  #dany wektor liczbowy
y <- 4                     #dana wartosc y

## ---- Operacje ----

n <- length(x)     #dlugosc x
z <- as.numeric(!findInterval(x,y, rightmost.closed=T))  #pokazuje, ktore 
                                                         #liczby z x sa 
                                                         #mniejsze badz 
                                                         #rowne y
l <- sum(z)     #liczy ile jest tych liczb
f <- l/n        #wartosc dystybuanty empirycznej

## ---- Prezentacja wyników ----

x     #dany wektor
y     #dana wartosc
f     #wartosc dystrybuanty empirycznej 

   

## ------------------------ Zadanie 01.13 ----------------------------

## ---- Dane wejsciowe ----

x <- round(runif(10,0,9))  #losuje jakis przykladowy wektor dlugosci 10 
                           #o wartosciach w {0,1,...,9}

top <- c(" _ ","  "," _ "," _ ","   "," _ "," _ "," _ "," _ "," _ ")
mid <- c("| |"," |"," _|"," _|","|_|","|_ ","|_ ","  |","|_|","|_|")
bot <- c("|_|"," |","|_ "," _|","  |"," _|","|_|","  |","|_|"," _|")

## ---- Operacje ----

y <- x+1    #bo w R indeksowanie jest od 1, a nie od 0

## ---- Prezentacja wyników ----

x     #moj przykladowy wektor
cat(top[y]); cat("\n"); cat(mid[y]); cat("\n"); cat(bot[y])



## ------------------------ Zadanie 01.14 ----------------------------

## ---- Dane wejsciowe ----

n1 <- 0:99    #bo ma byc 100 liczb, a zaczynamy od zera
n2 <- 0:999
n3 <- 0:9999

## ---- Operacje ----

p1 <- sum((4*(-1)^n1)/(2*n1+1))          #pi ze wzoru Leibniza dla 100 
                                         #poczatkowych wyrazow
p2 <- sum((4*(-1)^n2)/(2*n2+1))          #dla 1000
p3 <- sum((4*(-1)^n3)/(2*n3+1))          #dla 10000
n <- c(p1,p2,p3)                        
eps <- abs(n-pi)                

## ---- Prezentacja wyników ----

n           # poszczegolne przyblizenia pi (kolejno dla 100, 1000, 10000)
eps         # bledy oszacowan dla poszczegolnych przyblizen pi


