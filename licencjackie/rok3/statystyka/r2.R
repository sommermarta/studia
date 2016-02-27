pierwszazmienna<-1
drugazmienna<-2

# jhabgfdvljb         ctrl+shift+c ------ komentarz wielu linijek
# dfasgh
# sbdhtsh
# srwhrsh

x<--1:10
x
numeric(10)
is.numeric(x) #czy s¹ tam liczby
y<-c(T,F,T)
y
z<-c("a","b")
z
is.character(z)  #czy jest to wektor napisow
is.logical(z)
g<-character
g

A<-matrix(1:9,nrow=3)
A

#listy

lista<-list(c(1,2,3),c("a"),T)
lista
str(lista)

lista<-list(numeryczny=c(1,2,3),napisow=c("a"),logiczny=T)
lista
lista[[1]][2]

lista$numeryczny



x<-1:5
y<-c(rep("K",3),rep("M",2))
y
z<-c(T,F,T,T,F)
z

ramka.danych<-data.frame(nr=x,p³eæ=y,czyg³osuje=z)
ramka.danych

ramka.danych$nr
ramka.danych[1]
ramka.danych[c(1,2),]
ramka.danych[,c(1,3)]
ramka.danych[1,1]

ramka.danych[,2]

x<-c(rep(c("a","b","d","f"),each=4),rep("c",5))
x
faktor<-factor(x)
faktor
levels(faktor)

table(faktor)  #zlicza, ile mamy wyst¹pieñ w ka¿dej kategorii























