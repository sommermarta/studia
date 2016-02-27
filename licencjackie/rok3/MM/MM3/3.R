#lab 2
#zad1
n <- 500
x <- runif(n,1,10)
sigma <- 0.5
eps <- rnorm(n,0,sigma)

y <- 2 + 3*sin(x) + log(x) + eps

#wykres rozproszenia
plot(x,y)

f=function(x){
  y <- 2 + 3*sin(x) + log(x)
}
curve(f,from=1,to=10,col="red",add=T)

#tworzymy zbiór danych
x1<-x
x2<-x^2
x3<-x^3
x4<-sin(x)
x5<-cos(x)
x6<-tan(x)
x7<-log(x)
Dane<-data.frame(y,x1,x2,x3,x4,x5,x6,x7)
names(Dane)<-c("y","x","x2","x3","sinx","cosx","tanx","logx")
head(Dane)
#dopasowujemy model
model1=lm(y~.,data=Dane)
summary(model1)

#kryterium BIC
modelB=step(model1,k=log(n),direction="backward")
summary(modelB)

#kryterium AIC
modelA=step(model1,k=2,direction="backward")
summary(modelA)

#dorysowanie estymowanej krzywej (liczno??æ!)
vA <- modelA$coef
vB <- modelB$coef
fA <- function(x){
  vA[1]+vA[2]*sin(x)+vA[3]*cos(x)+vA[4]*log(x)
}
fB <- function(x){
  vB[1]+vB[2]*sin(x)+vB[3]*log(x)
}
plot(x,y)
curve(fA,from=1,to=10,add=T,col='green')
curve(fB,from=1,to=10,add=T,col='blue')

#ró¿nica funkcji kryterialnej
AIC(model1)-AIC(modelA)

#########################################################################
#zad2
# w zadaniu drugim jest b³šd!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# USMa
install.packages("AER")
library(AER)
data(USMacroG)
head(USMacroG)
USMacroG


##########################################################################
# zad4
library(faraway)
library(car)
sav<-read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/MM/DANE/savings.txt",header=T)
head(sav)
models=lm(Savings~Pop15+Pop75+dpi+ddpi,data=sav)

#wykres zmiennycg w ramce danych sav(parami bez nazwy kraju)
pairs(sav[,-1])
vif(models)

#kowariancja zmiennych
cor(sav[,-1])

# usuwamy zmienne skorelowane z Pop15(dpi oraz Pop75)
models1=lm(Savings~Pop15+ddpi,data=sav)
summary(models1)

#VIF - ile razy var(betaj)jest wiêksza, ni¿ gdyby zmiennna xj by³a  nieskorelowana z pozosta³ymi

vif(models1)
#wykres czê??ciowych rezyduów

prplot(models,1)
# library(graphics)
# matplot(models1)

model11 <- lm(Savings~Pop15+ddpi ,data=sav,subset = Pop15 <33)
model22 <- lm(Savings~Pop15+ddpi,data=sav,subset = Pop15 >33)
summary(model11)
summary(model22)

#################################################################
#zad3

data(USPop)
head(USPop)

b1=350
b2=4.5
b3=-0.3
time=0:21
plot(USPop$year,USPop$population)
#nieliniowe najmniejsze kawadraty
?nls

modelpop=nls(population~beta1/(1+exp(beta2+beta3*time)),start=list(beta1=350,beta2=4.5,beta3=-0.3),data=USPop,trace=T)

#jesli zmienimy betê 3  na 3 to bêdzie b³šd, czy co?? tam 

str(summary(modelpop))
summary(modelpop)

wsp=summary(modelpop)$coefficients

fP=function(x){
  wsp[1,1]/(1+exp(wsp[2]+wsp[3]*x))
}

plot(time,USPop$population)
curve(fP,from=0,to=22,add=T)

#predykcje dla roku 2015 - time=22.5
predict(modelpop,list(time=22.5))
#dopasowanie
plot(residuals(modelpop)~fitted(modelpop))  #ojej ;(
plot(residuals(model1)~fitted(model1))
fp(22.5)  #to samo, inny sposób
#chmura punktów -> dobre dopasowanie


#zad 5 - zadanie domowe

#a1
n <- 20
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)

sigma <- 1

eps <- rnorm(n,0,sigma)

y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps; y
beta <- c(0.5,2,1,0.5,0.3)

D <- data.frame(y,x1,x2,x3,x4)
names(D) <- c("y","x1","x2","x3","x4")
head(D)

mod <- lm(y~.,data=D)
summary(mod)
ws <- mod$coef; ws
wsp <- as.numeric(mod$coef); wsp

#a2
L <- 50

bla <- replicate(L,{
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)
  
  eps <- rnorm(n,0,sigma)
  
  y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps
  D <- data.frame(y,x1,x2,x3,x4)
  mod <- lm(y~.,data=D)
  wsp <- as.numeric(mod$coef)
  sum((wsp-beta)^2)
  
})

blad <- numeric(50)
blad
blad[1]
for(i in 1:L){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x4 <- rnorm(n)
  
  eps <- rnorm(n,0,sigma)
  
  y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps
  D <- data.frame(y,x1,x2,x3,x4)
  mod <- lm(y~.,data=D)
  wsp <- as.numeric(mod$coef)
  blad[i] <- sum((wsp-beta)^2)
}
blad
mean(blad)

bla
mean(bla)





#pd3

sigma <- c(0.5,1,2,3)
N <- seq(20,300,10); N
a <- matrix(numeric(length(sigma)*length(N)), nrow=length(N)); a


for(j in 1:length(sigma)){
  for(n in N){
    for(i in 1:L){
      x1 <- rnorm(n)
      x2 <- rnorm(n)
      x3 <- rnorm(n)
      x4 <- rnorm(n)
    
      eps <- rnorm(n,0,sigma[j])
    
      y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps
      D <- data.frame(y,x1,x2,x3,x4)
      mod <- lm(y~.,data=D)
      wsp <- as.numeric(mod$coef)
      blad[i] <- sum((wsp-beta)^2)
    }
    a[n/10-1,j] <- mean(blad)
  }
}
a
matplot(N, a, type="l")

#b
n <- seq(20,300,10); n
bb <- numeric(length(n))
for(i in 1:length(n)){
  b <- replicate(L,{
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    x4 <- rnorm(n)
    
    eps <- rnorm(n,0,sigma)
    
    y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps
    D <- data.frame(y,x1,x2,x3,x4)
    mod <- lm(y~.,data=D)
    wsp <- as.numeric(mod$coef)
    sum((wsp-beta)^2)
    
  })
  
  bb[i] <- mean(b)
}


par(mfrow=c(2,2))

sigma <- 2

plot(n,bb)



















