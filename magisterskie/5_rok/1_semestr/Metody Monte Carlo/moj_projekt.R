# projekt nr 1.2 na MMC - Marta Sommer

runif(100)   # GenerujU()

# algorytm:

n <- 5000
alg <- numeric(n)

for(i in 1:n){
   alg[i] <- sum(runif(12))-6
}

mean(alg)   # srednia
sd(alg)     # odchylenie standardowe
var(alg)    # wariancja

curve(dnorm, col="red", lwd=2, xlim=c(-6,6))  # teoretyczna gestosc rozkladu 
                                              # N(0,1)
hist(alg,freq=FALSE,add=TRUE)  # histogram dla naszych wygenerowanych danych

boxplot(alg, horizontal=TRUE)  # boxplot dla naszych wygenerowanych danych

# testy na normalnosc:

qqnorm(alg)   # wykres kwantylowy
abline(a=0,b=1,col="red")

shapiro.test(alg)   # test shapiro-wilka

pearson.test(alg)   # test chi2 Pearsona

# install.packages("nortest")
library("nortest")
cvm.test(alg)       # test Cramera von Misesa





