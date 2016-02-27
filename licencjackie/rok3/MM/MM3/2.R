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

#dorysowanie estymowanej krzywej (licznoœæ!)
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

install.packages("AER")
library(AER)
library(car)

data(USMacroG)
USMacroG

MacroDiff=apply(USMacroG,2,diff)
MacroDiff
Macrodiff=data.frame(MacroDiff)
Macrodiff

pairs(MacroDiff[,c(-1,-3,-7,-8,-10,-11,-12)])    #wykres rozproszenia
# liniowy wp³yw na consumption ma dpi

model=lm(consumption~dpi+cpi+government+unemp,data=Macrodiff)
modelA=step(model,k=2,direction="backward")
summary(modelA)                                   #wyrzuci³ najpierw government, potem cpi, a zostawi³ unemp i cpi

AIC(model)-AIC(modelA)                          #ró¿nice miêdzy wielkoœciami funkcji kryterialnej
cor(MacroDiff[,c(-1,-3,-7,-8,-10,-11,-12)])      # korelacje
vif(model)                                       #wspó³czynniki podbicia wariancji 




##########################################################################
# zad4
library("faraway")
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
#wykres czêœciowych rezyduów
prplot(models1,1)



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

#jesli zmienimy betê 3  na 3 to bêdzie b³¹d, czy coœ tam 

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


















