#Laborki 4, kartka 3
#zad1
x20<-rnorm(20)
x100<-rnorm(100)

par(mfrow=c(2,1))

curve(pnorm(x),from=-4, to=4, main="x20")
plot(ecdf(x20), add=T, col="red", xlim=c(-4,4))      #dystrybuanta empiryczna to ecdf

curve(pnorm(x),from=-4, to=4, main="x100")
plot(ecdf(x100), add=T, col="red", xlim=c(-4,4))

#zad2
#a
for(i in 1:5){    #æwiczymy pêtlê
  print(i)
}

suma=0           #dalej æwiczymy ;p
for(i in 1:5){
  suma<-suma+1
}
suma
i

#koniec æwiczeñ, wracamy do zadania 

n <- 500
x <- rcauchy(n)

sr <- numeric(n)
med <- numeric(n)
sr
med

for(i in 1:n){
  sr[i]<-mean(x[1:i])
  med[i]<-median(x[1:i])
}

sr[1:9]
med[1:9]

#wykresy

par(mfrow=c(1,1))
plot(1:i, sr, type="l", col="blue", ylab="", xlab="", lwd=1.5)
lines(1:i, med, col="green")
lines(1:i, rep(0,n), col="red")
legend("bottomright", c("sr_i", "med_i", "a=0"), col=c("blue", "green", "red"), lty=1)

#b

od<-numeric(n-1)
sqr<-numeric(n-1)

#bêdziemy przechodzic od drugiego elementu, bo nie ma odchylenia standarowego z jednej liczby

for(i in 2:n){
  od[i-1]<-sd(x[1:i])
  sqr[i-1]<-IQR(x[1:i])/2
}

plot(2:i, od, type="l", col="blue", ylab="", xlab="", lwd=1.5)
lines(2:i, sqr, col="green")
lines(1:i, rep(0,n), col="red")
legend("bottomright", c("od_i", "sqr_i", "a=0"), col=c("blue", "green", "red"), lty=1)


#zad3

n <- 1000
x <- rnorm(n)

sr <- numeric(n)
med <- numeric(n)
sr
med

for(i in 1:n){
  sr[i]<-mean(x[1:i])
  med[i]<-median(x[1:i])
}

sr[1:9]
med[1:9]

#wykresy

par(mfrow=c(1,1))
plot(1:i, sr, type="l", col="blue", ylab="", xlab="", lwd=1.5)
lines(1:i, med, col="green")
lines(1:i, rep(0,n), col="red")
legend("bottomright", c("sr_i", "med_i", "a=0"), col=c("blue", "green", "red"), lty=1)

#b

od<-numeric(n-1)
sqr<-numeric(n-1)

#bêdziemy przechodzic od drugiego elementu, bo nie ma odchylenia standarowego z jednej liczby

for(i in 2:n){
  od[i-1]<-sd(x[1:i])
  sqr[i-1]<-IQR(x[1:i])/2
}


#jak zapisac wykres do pdfa 

pdf("H:\\Windows7\\Desktop\\R\\rys.pdf")

plot(2:i, od, type="l", col="blue", ylab="", xlab="", lwd=1.5)
lines(2:i, sqr, col="green")
lines(1:i, rep(0,n), col="red")
legend("bottomright", c("od_i", "sqr_i", "a=0"), col=c("blue", "green", "red"), lty=1)

dev.off()   # to, co pomiêdzy tymi komendami zapisze siê do pdfa

#zad4
#zrobimy to nie forem, ale jakimœ replicatem, bo ponoæ jest szybciej

n<-1000
m<-20
teta<-1

#funkcja replicate

estymatory<- matrix(nrow=2,ncol=n, dimnames=list(c("emm","enw")))
estymatory[,1:3]

estymatory<-replicate(n,{    # n razy zrobi to, co w nawiasie klamrowym
  x<-runif(m,0,teta)     #wylosuje próbke
  c(2*mean(x),max(x))    #policzy
})

estymatory[,1:3]   #wyniki pierwszych trzech eksperymentów
dim(estymatory)

T1<-estymatory[1,]
T2<-estymatory[2,]

#obci¹¿enie
mean(T1)-1
mean(T2)-1   #wartosc oczekiwana - teta

var(T1)-mean(T1-1)^2
var(T2)-mean(T2-1)^2




















































