library("foreign")
library("survival")
library("rms")

dane <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\2\\newcyfra.dta")[,-1]
head(dane)

m0 <- coxph(Surv(dftime,dfree)~1, data=dane)
mart <- resid(m0)
plot(dane$cyfra,mart)
lines(lowess(dane$cyfra,mart,iter=0,f=0.6))
# widac, ze przeksztalcenie logarytmiczne byloby odpowiednie
plot(log(dane$cyfra),mart)
lines(lowess(log(dane$cyfra),mart,iter=0,f=0.6))
# zmiana gdzies w okolicach 1 i 2
exp(1); exp(2) # taki przyjmiemy sobie prog w takim razie

cyfra.new <- numeric(length(dane$cyfra))
for(i in 1:length(dane$cyfra)){
   if(dane$cyfra[i] <= exp(1)) cyfra.new[i] <- 0 else 
      if(dane$cyfra[i] <= exp(2)) cyfra.new[i] <- 1 else cyfra.new[i] <- 2
}
dane <- cbind(dane,cyfra.new)

km <- survfit(Surv(dftime,dfree)~log(cyfra.new), data=dane)
plot(km,col=c("red","blue","green"))
# im wyzszy wskaznik cyfry, tym gorzej - wyrazna roznica

# doloczmy teraz pozostale zmienne do modelu 
# (pomijam te podgrupy histpat i newtnm)

m1 <- coxph(Surv(dftime,dfree)~cyfra, data=dane)
m1

m2 <- coxph(Surv(dftime,dfree)~log(cyfra)+wiek+plec+ps+histpat+newtnm, 
            data=dane)
m2  # wiek, plec i ps nieistotne

km_ps <- survfit(Surv(dftime,dfree)~ps, data=dane)
plot(km_ps,col=c("red","blue"))  # ps mimo wszystko zostawie, bo wyglada na istotna

m3 <- coxph(Surv(dftime,dfree)~log(cyfra)+ps+histpat+newtnm, 
            data=dane)
m3

m4 <- coxph(Surv(dftime,dfree)~cyfra.new+histpat+newtnm, 
            data=dane)
m4

# czwarty najlepszy -> sprawdzmy teraz zalozenia:

plot(km, col=c("red","blue","green"),
     fun=function(x) log(-log(x)), log="x", firstx=10)  # z rownolegloscia nie jest najlepiej

plot(km_ps, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=10)  # tu moze troche lepiej

m1s <- cox.zph(m1, transform=function(x) log(x))
m1s    # czyli zalozenie jest spelnione
plot(m1s, df=4, nsmo=10, se=TRUE)
abline(0,0,lty=3,col="red")

m4s <- cox.zph(m4, transform="identity")
m4s    # czyli zalozenie jest spelnione dla histpat
plot(m4s, df=4, nsmo=10, se=TRUE,var=1)
abline(0,0,lty=3,col="red")

# weibull:

w1 <- survreg(Surv(dftime,dfree)~wiek +plec +ps +cyfra
              +adeno +large +plano +tnm1 +tnm2 +tnm3, 
              data=dane, dist="weibull")
print(w1)
p <- 1/w1$scale
p
time.w1 <- exp(w1$coefficients)
time.w1

w2 <- psm(Surv(dftime,dfree)~ ps +cyfra +adeno +large +tnm1 +tnm2 , 
              data=dane, dist="weibull",y=TRUE)
res.w2 <-resid(w2,type="cens")

plot(survfit(res.w2~1),conf="none",ylab="Survival probability (Gumbel)",
     xlab="Residual")
lines(res.w2, col="red")
time.w2 <- exp(w2$coefficients)
time.w2
qqnorm(res.w2[,1])
abline(-1.5,1)


w3 <- psm(Surv(dftime,dfree)~ cyfra.new +adeno +large +tnm1 +tnm2 , 
          data=dane, dist="weibull",y=TRUE)
res.w3 <-resid(w3,type="cens")

plot(survfit(res.w3~1),conf="none",ylab="Survival probability (Gumbel)",
     xlab="Residual")
lines(res.w3, col="red")
time.w3 <- exp(w3$coefficients)
time.w3


##############norm:

w1 <- survreg(Surv(dftime,dfree)~wiek +plec +ps +cyfra
              +adeno +large +plano +tnm1 +tnm2 +tnm3, 
              data=dane, dist="lognormal")
print(w1)
p <- 1/w1$scale
p
time.w1 <- exp(w1$coefficients)
time.w1

w1 <- survreg(Surv(dftime,dfree)~  cyfra
              +adeno +large +tnm1 +tnm2 , 
              data=dane, dist="lognormal")
print(w1)
p <- 1/w1$scale
p
time.w1 <- exp(w1$coefficients)
time.w1

w2 <- psm(Surv(dftime,dfree)~ cyfra +adeno +large +tnm1 +tnm2 , 
          data=dane, dist="lognormal",y=TRUE)
res.w2 <-resid(w2,type="cens")
plot(survfit(res.w2~1),conf="none",ylab="Survival probability (Gumbel)",
     xlab="Residual")
lines(res.w2, col="red")
time.w2 <- exp(w2$coefficients)
time.w2

qqnorm(res.w2[,1])
abline(-0.75,1)


install.packages("flexsurv")
library("flexsurv")



?flexsurv

w2 <- psm(Surv(dftime,dfree)~ ps +log(cyfra) +adeno +large +tnm1 +tnm2 , 
          data=dane, dist="weibull",y=TRUE)
time.w2 <- exp(w2$coefficients)
time.w2

   par(mfrow=c(1,2))
res.w2 <-resid(w2,type="cens")
plot(survfit(res.w2~1),conf="none",ylab="Survival probability (Gumbel)",
     xlab="Residual", main="Rysunek 5")
lines(res.w2, col="red")
qqnorm(res.w2[,1],main="Rysunek 6")
abline(-1.5,1)
par(mfrow=c(1,1))


