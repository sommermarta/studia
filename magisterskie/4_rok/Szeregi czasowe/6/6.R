# zad.1

library("tseries")

# a)

xt <- numeric(1100)
zt <- rnorm(1100)
alfa0 <- 0.1
alfa1 <- 0.5
alfa2 <- 0.2

xt[1:2] <- rnorm(2,0,alfa0/(1-alfa1-alfa2))

skwt <- numeric(1100)
for(i in 3:1100){
  skwt[i] <- alfa0 + alfa1*xt[i-1]^2 + alfa2*xt[i-2]^2
  xt[i] <- sqrt(skwt[i])*zt[i]
}

xt <- xt[101:1100]
ts.plot(xt)
acf(xt)

# b)

Box.test(xt,lag=20,type="Ljung")     
# nie wykrywa zaleznosci ARCH!!!!!!!! bo on wykrywa tylko zaleznosc linowa
Box.test(xt^2,lag=20,type="Ljung")  
# a tu jest zaleznosc nielinowa! i tu juz wykrywa, dlatego, gdy chcemy spr,
# czy jest efekt arch lub garch warto testowac kwadraty

# c)

mod <- arima(xt^2,c(2,0,0))
Box.test(mod$residuals,lag=20,type="Ljung")  
# a teorerycznie powinien byc ar(2), wiec jest ok :D

# d)

arch <- garch(xt,order=c(0,2),trace=FALSE)    # order(garch, arch) stopien
summary(arch)$coef   # mniej wiecej wychodza wartosci teoretyczne

# e)

logLik(arch)  # logarytm wiarogodnosci
AIC(arch)

# f)

fit <- fitted(arch)
plot(fit[,1]^2,type="l",ylab="Conditional Variance")  
# wykres warunkowej wariancji
# jaka mielismy wariancje w czasie t pod warunkiem wczesniejszego momentu

# g)

ga <- garch(xt,order=c(1,1),trace=FALSE)
summary(ga)
# jaque bera test -> test na noramlnosc reziduow
# ljunga boxa dla kwadratow reziduow
summary(ga)$coef 

# h)

AIC(ga)
AIC(arch)

# lepszy jest ten, ktory ma mniejsza wartosc AIC


# zad.3

# O nieadekwatnosci modelowania zwrotow na
# podstawie liniowych modeli autoregresyjnych

library("evir")

data(bmw,package="evir")
# bmw - wektor zwrotow logarytmicznych

head(bmw)
bmw <- as.vector(bmw)
n <- length(bmw)

acf(bmw)
pacf(bmw)
plot(bmw)

for (p in 0:3) {
  for (q in 0:3) {
    a <- AIC(arima(bmw,c(p,0,q)),k=log(n))
    print(c(p,q,a))
  }
}
# metoda na oko - minimum sugeruje model MA(1)

fitMA1 <- arima(bmw, order = c(0,0, 1))

Box.test(fitMA1$resid,lag=20,type="Ljung")
Box.test(fitMA1$resid^2,lag=20,type="Ljung")  # wskazuje na szereg ARCH

acf( residuals(fitMA1),lag.max=20)
qqnorm(residuals(fitMA1),datax=T,main="MA(1) resid")
plot(residuals(fitMA1),ylab="Residual")

# rozklad rezyduów nie jest normalny
# widoczne skupienia zmiennoœci => rezydua s¹ zale¿ne


# dopasowujemy model MA(1) + rezydua GARCH(1,1), rozklad warunkowy normalny

library("fGarch")

bmw.garch_norm <- garchFit(~arma(0,1)+garch(1,1), data=bmw, 
                          cond.dist="norm",trace=FALSE)
# dopasuje arma do danych a garch do residuow
summary(bmw.garch_norm)  # ljung box -> to Q() oznacza jaki lag

# wykres kwantylowy dla rezyduów

x <- bmw.garch_norm@residuals / bmw.garch_norm@sigma.t
plot(x)  
qqnorm(x,datax=T,ylab= "Standardized residual quantiles",
       main=" normal plot", xlab="normal quantiles")
qqline(x,datax=T)

# duze odstepstwa od normalnosci - grubsze ogony.
# Dopasowujemy rozklad t do rezyduow.
# Wykonujemy qqplot  dla rozkladu t o 4 st. sw.

grid = (1:n)/(n+1)
qqplot(sort(x), qt(grid,df=4),
       main= " t plot, df=4",xlab= "Standardized residual quantiles",
       ylab="t-quantiles")
abline(   lm(   qt(c(.25,.75),df=4)~quantile(x,c(.25,.75))   )   )


# zmieniamy rozklad warunkowy na t

bmw.garch_t <- garchFit(~arma(0,1)+garch(1,1),cond.dist="std",
                        data=bmw,trace=FALSE)

options(digits=4)
summary(bmw.garch_t)  # parametr shape-> stopnie swobody.

loglik_bmw <- bmw.garch_t@fit$llh # -loglik dla modelu bmw.garch_t

BIC_bmw_t <- 2*loglik_bmw+log(n)*6
as.numeric(BIC_bmw_t) # wartoœæ kryterium BIC dla tego modelu
# lepiej (mniej) niz w modelu ma cos dopasowanym na poczatku tego zadania

# zad.2

# 1)

x <- read.table("http://gamma.mini.pw.edu.pl/~szymanowskih/lab6/exch1.txt")
head(x,2)
ts.plot(x)
acf(x)
pacf(x)
Box.test(x,lag=20,type="Ljung")
Box.test(x^2,lag=20,type="Ljung")
# te powyzsze testy to bardzo charakterystyczna rzecza dla efektu arch
# dla zwyklych danych nic sie nie dzieje, a dla kwadratow jest juz problem

xt <- as.numeric(as.matrix(x))

# 2)

plot(density(xt))
curve(dnorm(x,mean(xt),sd(xt)),add=T,col="red")

# 4)

for(i in 0:3){
  for(j in 1:3){
    a <- AIC(garch(xt,order=c(i,j),trace=FALSE),k=log(length(xt)))
    print(c(i,j,a))
  }
}

# szukam minimum

