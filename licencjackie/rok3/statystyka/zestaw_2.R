#zad1
 
licz <- c(17364,56128,11239,8170)
stan <- c("panny", "mê¿atki", "wdowy", "rozwódki")

# -------- wykres ko³owy ----------

pie(licz, main="stan cywilny kobiet, 1988, USA", col=rainbow(4),
    labels=paste(stan,licz, sep=" - "))
pie(licz, main="stan cywilny kobiet, 1988, USA", col=rainbow(4),
    labels=licz)
legend(-2.2,-0.2,stan, fill=rainbow(4))

# -------- wykres s³upkowy --------

x <- barplot(licz, names=stan, col=rainbow(4), ylim=c(0,65000),
        main="stan cywilny kobiet, 1988, USA")
text(x, licz + 4000, as.character(licz))
y <- barplot(licz, names=stan, col=rainbow(4), xlim=c(0,65000),
        main="stan cywilny kobiet, 1988, USA", horiz=T)

#zad2

stac <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok3\\statystyka\\stacje.csv",
                   header=T)
head(stac)
head(stac$Answers)
stacj <- factor(stac$Answers)
length(stacj)
stacje <- table(stacj)
print(stacje)

kier <- names(stacje)
print(kier)
liczb <- as.vector(stacje)
print(liczb)

pie(liczb,col=heat.colors(4),labels=paste(kier,liczb,sep=" - "),
    main="odpowiedzi kierowców")

z <- barplot(liczb, names=kier, ylim=c(0,400), col=heat.colors(4),
        main="odpowiedzi kierowców")
text(z, liczb+25, as.character(liczb))



#zad3

cen <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 25.20, 24.50, 24.60, 
         24.10, 24.30, 26.10, 23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 
         25.20, 26.80)
plot(1:20, cen)
plot(cen)       # to samo, co wy¿ej 
plot(1:20, cen, type="l")
plot(1:20, cen, type="b", pch=20, lty=3, main="Notowania akcji pewnej
     spó³ki w PLN", xlab="dzieñ", ylab="cena [z³]")


#zad4

but <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok3\\statystyka\\butelki.csv",
                        header=T)
head(but)

bute2 <- but[,1]    
length(bute2)

#a

cis <- bute2 * 0.0068947
cis

#b

hist(cis, main="histogram opisuj¹cy wytrzyma³oœæ butelek", xlab="klasy", 
     ylab="czêstoœæ wystêpowania", col=rainbow(7))

par(mfrow=c(2,2))    #tworzy 2 wykresy w kolumnie i dwa wykresy w wierszu
hist(cis, col=rainbow(7), xlab="", ylab="")
hist(cis, breaks=100, col=rainbow(100), xlab="", ylab="")
hist(cis, breaks="Scott", col=rainbow(100), xlab="", ylab="")
hist(cis, breaks="FD", col=rainbow(20), xlab="", ylab="")

par(mfrow=c(1,1))

#c - ³amana licznoœci
h <- hist(cis, main="histogram opisuj¹cy wytrzyma³oœæ butelek", xlab="klasy", 
          ylab="czêstoœæ wystêpowania", col=rainbow(7))
str(h)     #parametry histogramu

b<-diff(h$mids)[1]    #szerokoœæ s³upka
b
k<-length(h$mids)     #liczba s³upków
k
x<-c(h$mids[1]-b,h$mids, h$mids[k]+b)  #wektor œrodków + dodatkowy punkt koñcowy i pocz¹tkowy dla ³amanej licznoœci
x
y<-c(0,h$counts,0)    #wysokoœci s³upków + tych dwóch dodatkowych
y
lines(x,y,col="black")   #nanoszenie krzywej na wykres
points(x,y,pch=20)        #nanoszenie punktów na wykres
hist(cis,prob=T)        #histogram czêstoœci

#d wykres ³odygowo-liœciowy

stem(cis)
?stem

#e wykres skrzynkowy

boxplot(cis)
boxplot(cis,horizontal=T)
boxplot(cis,horizontal=T, col="green",main="Wykres skrzynkowy - butelki")

#f podstawowe statystyki próbkowe dla danych opisuj¹cych wytrzyma³oœæ butelek

mean(cis)        #œrednia arytmetyczna
median(cis)     #mediana        
min(cis)        # minimalna wartoœæ
max(cis)         #maksymalna wartoœæ
range(cis)      #zarówno minimum, jak i maximum
quantile(cis)    #podstawowe kwantyle

var(cis)     #wariancja
sd(cis)     #odchylenie standardowe
IQR(cis)    #rozstêp miêdzykwartylowy
sd(cis)/mean(cis)    #wspó³czynnik zmiennoœci

install.packages("e1071")
library("e1071")

skewness(cis)    #wspó³czynnik skoœnoœci
kurtosis(cis)    #kurtoza

#g percentyle rozwa¿anych danych

quantile(cis,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95))

#h œrednia uciêta 10%

mean(cis,trim=0.1)
mean(cis)


#zad 5

x<-c(334,352,384,436,405,498,425,392,374,398,403,389,424,344,367,429,
     400,457,392,424,409,428,443,454,339,378,345,389,387,422)
m<-hist(x); m
str(m)
r<-m$mids[2]-m$mids[1]; r
l<-length(m$mids); l
a<-c(m$mids[1]-r,m$mids,m$mids[l]+r); a
b<-c(0,m$counts,0); b
lines(a,b,col="red")
points(a,b,pch=20, col="red")

boxplot(x, horizontal=T)
mean(x)
median(x)
sd(x)
range(x)
quantile(x)

#zad6

sam <- read.csv2("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok3\\statystyka\\samochody.csv")
head(sam)
dim(sam)        #wymiary danych

#a
zp <- (1/sam$mpg)*(3.785*100/1.609)
zp    # widzimy, ¿e jest tu jakiœ brak danych, wiêc chcemy go usun¹æ
zpp<-na.omit(zp)
zpp
class(zpp)    #upewniamy siê, ¿e s¹ tam same liczby

#b
stem(zpp)

#c
hist(zpp, main="histogram zu¿ycia paliwa", col=rainbow(11))

#d ???????????????????????????????????

#e
boxplot(zpp, horizontal=T)

#f
mean(zpp)
median(zpp)
var(zpp)
sd(zpp)
IQR(zpp)
quantile(zpp)
range(zpp)
max(zpp)-min(zpp)
sd(zpp)/mean(zpp)   #wspó³czynnik zmiennoœci

library("e1071")
kurtosis(zpp)      #kurtoza
skewness(zpp)      #wspó³czynnik asymetrii

#g
quantile(zpp,c(0.05,0.1,0.9,0.95))

#h
mean(zpp,trim=0.05)

#zad7
spal <- character(length(zpp))    #tworzê pusty wektor napisów d³ugoœci zpp
spal
spal[zpp<=7] <- "malo"                  #przypisujê kolejnym wartoœciom ich katogorie
spal[zpp>7 & zpp<=10] <- "srednio"
spal[zpp>10] <- "duzo"
spal

spall <- factor(spal)  #konwersja na zmienn¹ jakoœciow¹
spall
table(spall)             #tabela licznoœci
barplot(table(spall))     #wykres licznoœci
prop.table(table(spall))    #tabela czêstoœci
barplot(prop.table(table(spall)))   #wykres czêstoœci

#zad8
head(sam)                  # chcemy wykresy skrzynkowe dla konkretnych producentów oddzielnie
boxplot(zp~sam$producent)     #!!!!!!!!!!!!! wa¿ne!!!!!!!!!!
boxplot(zp[which(sam$producent==1)])    #tylko dla pierwszego producenta

a <- zp[sam$producent==1]  ;a
mean(a); sd(a)
b <- zp[sam$producent==2]  ;b
bb <- na.omit(b)
mean(bb); sd(bb)
c <- zp[sam$producent==3]  ;c
mean(c); sd(c)

length(a)+length(b)+length(c)   #ma³e sprawdzenie ;p zgadza siê!!!

#zad9
head(sam)
str(sam)
cyl <- sam$cylindry
cyl
cyll <- factor(cyl)
cyll
a <- zp[sam$cylindry==3];  a
b <- zp[sam$cylindry==4];  b 
c <- zp[sam$cylindry==5];  c
d <- zp[sam$cylindry==6];  d
e <- zp[sam$cylindry==8];  e

length(a)+length(b)+length(c)+length(d)+length(e)
boxplot(zp~sam$cylindry, horizontal=T)

#zad10 
head(sam)
gg <- zp[sam$waga <2500]
gg
length(gg)
mean(gg)
sd(gg)
median(gg)
var(gg)
skewness(gg)

#zad11
head(sam)

a <- sam$moc[sam$rok>=79 & sam$rok<=81];  a
aa <- na.omit(a2);  aa
boxplot(a3, horizontal=T)
quantile(aa,0.95)

#zad12
head(sam)

b <- sam$przysp[sam$waga>=2500 & sam$waga<=3000]; b
boxplot(b, horizontal=T)
quantile(b, 0.75)

#zad13
head(sam)
c <- sam$waga[sam$mpg>=26]; c
cc <- na.omit(c)
boxplot(cc, horizontal=T)
quantile(cc, 0.95)

#zad14
A <- sam$przysp[sam$producent==1]; A
J <- sam$przysp[sam$producent==3]; J

boxplot(A, horizontal=T)
boxplot(J, horizontal=T)
boxplot(A,J, horizontal=T)

#cudowne chwile w zaciszu domowym:
#zad1
b <- rnorm(15, 0, 1)
hist(b)




