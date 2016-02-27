
dane <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Ekonometria\\spr5\\wybrane2.csv",
                   header=T, sep=";", dec=",")
head(dane)
dane
dane<-dane[-c(2468:2472),] # ucinam 4 ostatnie obserwacje, bo nie zrobie dla nich interpolacji

library("zoo")
library("quantmod")
library("tseries")
library("fUnitRoots")


dane<-na.approx(dane) # interpolacja liniowa brakow danych
dane<-as.data.frame(dane)
attach(dane)
head(dane)
dane

#1

apply(dane,2,adfTest) # indeksy sa niestacjonarne
matplot(dane,type="l")

# logarytmiczne stopy zwrotu, poczawszy od t=2,...
logprice<-apply(dane,2,function(x){  
   log(Lag(x,k=0)/Lag(x,k=1))
})[-1,]

logprice

logprice<-as.data.frame(logprice)
names(logprice)<-c("logWIG","logDAX","logCAC40","logFTSE")
head(logprice)


apply(logprice,2,adfTest) # odrzucamy H0, logstopy zwrotu sa stacjonarne
matplot(logprice,type="l")

#1 bis


logprice
# zainstaluj pakiet
library("vars")
var<-VAR(logprice) # model VAR(1)
summary(var)


#2
# HAC poki co pomijam, bo babka sama nie wiedziala co to jest...
library("HAC")

p<-1:10 # p moze byc dowolne
crit<-matrix(0,ncol=2,nrow=length(p),dimnames=list(NULL,c("AIC","BIC")))
crit

for (i in p){
   var<-VAR(logprice,p=i)
   crit[i,1]<-AIC(var)  
   crit[i,2]<-BIC(var)   
}

crit

which(crit[,1]==min(crit[,1])) # AIC wskazuje na opoznienie =5
which(crit[,2]==min(crit[,2])) # BIC wskazuje na opoznienie =1
# zbadamy autokorelacje reszt dla VAR(1)


var1<-VAR(logprice,p=1)
res1<-residuals(var1)
head(res1) # reszty modelu 
apply(res1,2,function(x){Box.test(x,type="Ljung")}) # reszty nieskorelowane, wiec wybieram model VAR(1) 

#3 

summary(var1) # przyczynowosc Grangera
# patrzec na testy t studenta i stad wnioskowac co jest zalezne a co nie


#4
# mimo, ze zwykle stopy sa niestacjonarne, trzeba zbudowac jakiĹ VAR, aby miec macierz oszacowan parametrow
# ktore zostana wykorzystane w VECM

p<-1:10
crit2<-matrix(0,ncol=2,nrow=length(p),dimnames=list(NULL,c("AIC","BIC")))

for (i in p){
   var<-VAR(dane,p=i)
   crit[i,1]<-AIC(var)  
   crit[i,2]<-BIC(var)   
}

crit

which(crit[,1]==min(crit[,1])) # AIC wskazuje na opoznienie =10 (bez przesady...), moze byc trudnosc 
# z interpretacja
which(crit[,2]==min(crit[,2])) # BIC wskazuje na opoznienie =2

# zbadamy autokorelacje reszt dla VAR(2)

var2<-VAR(dane,p=2)
res2<-residuals(var2)
head(res2) # reszty modelu 
apply(res2,2,function(x){Box.test(x,type="Ljung")}) # reszty nieskorelowane,
# wiec wybieram model VAR(2) 

# 5 nie wiem czy trzeba robic dla zwyklych szeregow czy log, przypuszczam ze dla zwyklych.
# liczba relacji kointegrujacych przyda sie dla VECM
var2


library("urca")
help("packages"=urca)
?ca.jo
joh<-ca.jo(dane,type="trace",K=2,ecdet="none")
summary(joh) # K=4, r=2 (rzad macierzy pi) wiec K-r=2, mamy 2 relacje integrujace



# 6

vec<-(cajorls(joh,r=2))
vec # beta mowi o relacjach kointegrujacych


