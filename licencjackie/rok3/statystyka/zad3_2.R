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

#zasd5

n <- 10000
m <- 10

alfa <- 0.05
q <- qt(1-alfa/2 , m-1)

czywpada <- replicate(n,{
  x<-rnorm(m,0,1)
  sr<-mean(x)
  s<-sd(x)
  (sr-q*s/sqrt(m)<0)&(sr+q*s/sqrt(m)>0)
})

czywpada[1:10]
sum(czywpada)/n


#zad6

n<-50
alfa<-0.05
q<-qnorm(1-alfa/2)
sr<-28.40
s<-4.75

c1<-sr-q*s/sqrt(n)
c2<-sr+q*s/sqrt(n)  
c1
c2

sr+c(-1,1)*q*s/sqrt(n)   #to samo, co wy¿ej, tylko inaczej zapisane

#zad7


n<-18
temp<-c(330,322,345,328.6,331,342,325.8,333,326.5,340,341,334,322.6,329.7,327.3,340.4,337.5,342.4)
sr<-mean(temp)
alfa<-0.05
q<-qt(1-alfa/2,n-1)
s<-sd(temp)
#dla œredniej
sr+c(-1,1)*q*s/sqrt(n)
#dla wariancji
q1<-qchisq(alfa/2,n-1)
q2<-qchisq(1-alfa/2,n-1)

sqrt((n-1)*s^2/q1)
sqrt((n-1)*s^2/q2)

#przedzia³ ufnoœci dla œredniej  normalny, o nieznanym sigma
t<-t.test(temp, conf.level=0.95)
str(t)
t$conf.int                #inny sposób

#zad8

n<-1014
p<-578/1014
p
alfa=0.05
q<-qnorm(1-alfa/2)

p+c(-1,1)*q*sqrt(p*(1-p)/n)

prop.test(578,n,conf.level=0.95)$conf.int   #jest inny, bo jest jakaœ korekta na ci¹g³¹œæ, mo¿na to wy³¹czyæ i wyjdzie:
prop.test(578,n,conf.level=0.95 ,correct=F)$conf.int   #coœ tu nie tak :(

#zad 10
data(iris)   #wczytujemy z Ra tê ramkê
str(iris)
head(iris)
#a
x<-iris$Petal.Length[which(iris$Species == "virginica")]
x
alfa<-0.01
t.test(x,conf.level=0.99)$conf.int
#b
alfa=0.05
n<-length(x)
n
q1<-qchisq(1-alfa/2,n-1)
q2<-qchisq(alfa/2,n-1)
s<-sd(x)

sqrt((n-1)*s^2/q1)
sqrt((n-1)*s^2/q2)

#zad11
n<-150
k<-19
p<-k/n
p
alfa<-0.04
q<-qnorm(1-alfa/2)

p+c(-1,1)*q*sqrt(p*(1-p)/n)

#zad9
n<-12
k<-3
alfa<-0.05
q<-qnorm(1-alfa/2)
p<-k/n
p+c(-1,1)*q*sqrt(p*(1-p)/n)

#zad12

data(chickwts)
head(chickwts)
alfa=0.07
x<-chickwts$weight[which(chickwts$feed == "soybean")]
x
n=length(x)
q1<-qchisq(1-alfa/2,n-1)
q2<-qchisq(alfa/2,n-1)
s<-sd(x)

sqrt((n-1)*s^2/q1)
sqrt((n-1)*s^2/q2)

#zad13
data(faithful)
head(faithful)
alfa=0.01
q<-qnorm(1-alfa/2)
sr<-mean(faithful$waiting)
s<-sd(faithful$waiting)
n<-length(faithful$waiting)
sr+c(-1,1)*q*s/sqrt(n)

#zad14

data(Orange)
head(Orange)
a<-0.01
x<-Orange$circumference
s<-sd(x)
n<-length(x)
q1<-qchisq(1-a/2,n-1)
q2<-qchisq(a/2,n-1)

sqrt((n-1)*s^2/q1)
sqrt((n-1)*s^2/q2)

#zad15
library("MASS")
data(Pima.te)
head(Pima.te)
a=0.05
x<-which(Pima.te$type=="Yes" & Pima.te$age>=35)
x
k<-length(x)

n<-length(Pima.te$age)
n
k
alfa<-0.05
q<-qnorm(1-alfa/2)
p<-k/n
p+c(-1,1)*q*sqrt(p*(1-p)/n)





















































