# biblioteki:

library("foreign")
library("survival")

# wstepna analiza danych:

b <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\projekt\\BreastFeeding.dta")
head(b)
names(b)[2] <- "koniec_karmienia"
n <- nrow(b)

rasa_biala <- ifelse(b$race==1,1,0)
rasa_czarna <- ifelse(b$race==2,1,0)

bb <- cbind(b[,-3],rasa_biala,rasa_czarna)
head(bb,2)

hist(bb$educ)
hist(bb$age)

modelpusty <- coxph(Surv(feed, koniec_karmienia) ~ 1, data = bb) 
mart <- resid(modelpusty)

plot(bb$educ, mart, xlab="Edukacja", ylab="Reszty martynga這we",pch=19) 
lines(lowess(bb$educ, mart, iter=0, f=0.6), col="red") 
# widac, ze zmienna ciagla nie potrzebuje zadnego przeksztalcenia

plot(bb$age, mart, xlab="Wiek", ylab="Reszty martynga這we", pch=19) 
lines(lowess(bb$age, mart, iter=0, f=0.6), col="red") 
# ta zmienna ciagla tez nie potrzebuje przeksztalcenia

# nasze ostateczne dane wygladaja wiec tak:

head(bb,3)

# sprawdzmy zalozenia modelu PH:

# najpierw krzywe Kaplana-Meiera:

km1 <- survfit(Surv(feed,koniec_karmienia) ~ econ, data=bb, conf.type="none")
plot(km1,col=c("red","blue"), lty=1:2, main="Krzywa K-M dla econ")

km2 <- survfit(Surv(feed,koniec_karmienia) ~ smok, data=bb, conf.type="none")
plot(km2,col=c("red","blue"), lty=1:2, main="Krzywa K-M dla smok")

km3 <- survfit(Surv(feed,koniec_karmienia) ~ alco, data=bb, conf.type="none")
plot(km3,col=c("red","blue"), lty=1:2, main="Krzywa K-M dla alco")

km4 <- survfit(Surv(feed,koniec_karmienia) ~ care, data=bb, conf.type="none")
plot(km4,col=c("red","blue"), lty=1:2, main="Krzywa K-M dla care")

km5 <- survfit(Surv(feed,koniec_karmienia) ~ rasa_biala, data=bb, conf.type="none")
plot(km5,col=c("red","blue"), lty=1:2, main="Krzywa K-M dla rasa_biala")

km6 <- survfit(Surv(feed,koniec_karmienia) ~ rasa_czarna, data=bb, conf.type="none")
plot(km6,col=c("red","blue"), lty=1:2, , main="Krzywa K-M dla rasa_czarna")

# krzywe przecinaja sie, ale sa bardzo zblizone, wiec raczej spelniaja
# zalozenia PH

# sp鎩rzmy na przykszta販one krzywe prze篡cia:

plot(km1,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla econ")
plot(km2,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla smok")
plot(km3,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla alco")
plot(km4,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla care")
plot(km5,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla rasa_biala")
plot(km6,col=c("red","blue"), lty=1:2, fun=function(x) log(-log(x)), log="x", firstx=1, main="Krzywa K-M log(-log()) dla rasa_czarna")

# dopasujmy model PH:

ph <- coxph(Surv(feed, koniec_karmienia)~., data=bb)
summary(ph)

# i jeszcze formalny test shoenfelda

test <- cox.zph(ph, transform="identity")
test   # zalozenia dla wszystkich zmiennych sa spelnione :D

# rezidua shoenfelda:

plot(test, df=3, nsmo=10, se=TRUE, var=1, main="Reszty Schoenfelda dla econ", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=2, main="Reszty Schoenfelda dla smok", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=3, main="Reszty Schoenfelda dla alco", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=4, main="Reszty Schoenfelda dla age", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=5, main="Reszty Schoenfelda dla educ", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=6, main="Reszty Schoenfelda dla care", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=7, main="Reszty Schoenfelda dla rasa_biala", pch=19)
abline(0, 0, lty=3, col="red")

plot(test, df=3, nsmo=10, se=TRUE, var=8, main="Reszty Schoenfelda dla rasa_czarna", pch=19)
abline(0, 0, lty=3, col="red")

# diagnostyka:

dewiancja <- residuals(ph, type="deviance")
coef <- ph$linear.predictors
mart


plot(1:n, dewiancja, pch=19, xlab="Numer", ylab="Reszty dewiancji")
abline(0,0, col="red", lwd=2)

plot(coef, dewiancja, pch=19, xlab="Liniowa kombinacja zmiennych", ylab="Reszty dewiancji")
abline(0,0, col="red", lwd=2)

plot(1:n, mart, pch=19, xlab="Numer", ylab="Reszty martynga這we")
abline(0,0, col="red", lwd=2)

plot(coef, mart, pch=19, xlab="Liniowa kombinacja zmiennych", ylab="Reszty martynga這we")
abline(0,0, col="red", lwd=2)



