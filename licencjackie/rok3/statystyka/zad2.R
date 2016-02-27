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

par(mfrow=c(2,2))  #dwa wykresy w wierszu i dwa wykresy w kolumnie

hist(cis)
hist(cis,breaks=100)  #ile chcemy klas
hist(cis,breaks=c(1.2,1.6,2,2.4,2.6))  #granice klas
hist(cis,breaks="Scott")
hist(cis,breaks="FD")



















