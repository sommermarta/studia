#zad1

licznosci<-c(17364,56128,11239,8170)
stan<-c("panny","mezatki","wdowy","rozwodki")

print(licznosci)

pie(licznosci)

pie(licznosci,col=c("red","blue","green","pink"),labels=licznosci)
legend("bottomleft",fill=c("red","blue","green","pink"),stan)

pie(licznosci,main="stan cywilny",col=heat.colors(4),labels=licznosci)

paste(stan,licznosci,sep="-")

pie(licznosci,main="stan cywilny",col=heat.colors(4),labels=paste(stan,licznosci,sep="-"))

#_________________________________ wykres s³upkowy

barplot(licznosci,names=stan,col=rainbow(4),ylim=c(0,65000),main="stan cywilny")

x<-barplot(licznosci,names=stan,col=rainbow(4),ylim=c(0,65000),main="stan cywilny")
x      #wspó³rzêdne x dla poszczególnych wierzcho³ków

text(x,licznosci,as.character(licznosci))
as.character(licznosci)

x<-barplot(licznosci,names=stan,col=rainbow(4),ylim=c(0,65000),main="stan cywilny")
text(x,licznosci+4000,as.character(licznosci))

prop.table(licznosci)
licznosci/(sum(licznosci))  #to samo

barplot(prop.table(licznosci),col=rainbow(4),main="stan cywilny",ylimm=c(0,1),legend.text=stan)
barplot(licznosci,horiz=T)

#zad4


butelki<-read.csv("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/butelki.csv")
butelki
?read.csv

head(butelki)
head(butelki,4)
is.numeric(butelki[,1])
dim(butelki)

#a

cis<-butelki[,1]*0.0068947
cis

#b

hist(cis)
h<-hist(cis)
h
str(h)


hist(cis)
hist(cis,breaks=100)  #ile chcemy klas
hist(cis,breaks=c(1.2,1.6,2,2.4,2.6))  #granice klas
hist(cis,breaks="Scott")
hist(cis,breaks="FD")

################################################
lab3

par(mfrow=c(1,1))  # z powrotem jeden obrazek

h<-hist(cis)
str(h)
b<-diff(h$mids)[1]
b
k<-length(h$mids)
k
x<-c(h$mids[1]-b,h$mids, h$mids[k]+b)
x
y<-c(0,h$counts,0)
y
lines(x,y,col="red")   #nanoszenie krzywej na wykres
points(x,y,pch=20,col="red")  #jeœli w pch="i tu jakiœ symbol to on siê wyœwietli"

hist(cis,prob=T)  #histogram czêstoœci


#wykres  ³odyga lliœcie

stem(cis)

# wykres skrzynkowy

boxplot(cis)
boxplot(cis,horizontal=T)


#podstawowe statystyki próbkowe

#charakterystyki po³o¿enia
mean(cis)
median(cis)
min(cis)
max(cis)
range(cis)   #zakres i minimum i maximum
quantile(cis)

#charakterystyki rozproszenia

var(cis)
sd(cis)
IQR(cis)

sd(cis)/mean(cis)  #wspó³czynnik zmiennoœci

install.packages("e1071")

#58 enter
library("e1071")     #za ka¿dym razem trzeba za³adowaæ, raz zainstalowaæ


skewness(cis)    #wspó³czynnik skoœnoœci
kurtosis(cis)    #kurtoza

quantile(cis)
quantile(cis,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95))  # kwantyle, które chcemy
mean(cis,trim=0.1)  #œrednia uciêta 10%


#zad6
sam<-read.csv2("http://www.ibspan.waw.pl/~pgrzeg/stat_lab/samochody.csv")
head(sam)

dim(sam)   #liczba wierszy i liczba kolumn

#a
# zp=(1/mpg)*((3.785*100)/1.609)

zp<-(1/sam$mpg)*((3.785*100)/1.609)
zp
#usuñmy brak danych ;p
zp2<-na.omit(zp)
zp2
class(zp2)

density(zp2)  #estymuje wartoœci numeryczne
plot(density(zp2))
plot(density(zp2),ylim=c(0,0.3),main="estymator gestosci")
lines(density(zp2,bw="nrd"),col=2)   #bw - zmienia nam j¹dro
lines(density(zp2,bw="ucv"),col=3)
lines(density(zp2,bw="SJ-ste"),col=4)

legend("topright",legend=c())   #niedokoñczone ;p

#zad 7

spal<-character(length(zp2))
spal
spal[zp2<7]<-"malo"
spal[zp2>=7 & zp2<=10]<-"srednio"
spal[zp2>10]<-"duzo"

spal<-factor(spal)
spal
table(spal)

barplot(table(spal))
barplot(prop.table(table(spal)))     #czêstoœci wykres

#zad8

head(sam)
#trik R-owy ;p
boxplot(zp~sam$producent)
boxplot(zp[which(sam$producent==1)])























