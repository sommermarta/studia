# 1

a <- read.csv2("H://Windows7//Desktop//Ekonometria//2//zad1.csv",header=TRUE,sep=";")
a
names(a) <- c("wydatki","dochod")
attach(a)

# a
l <- lm(wydatki~dochod, data=a)
summary(l)
plot(l,1)

#b
l2 <- lm(log(wydatki)~log(dochod),data=a)
summary(l2)
plot(l2,1)
# poprawilo sie troche, czasami przeksztalcenie zmiennych cos poprawia

# c

install.packages("lmtest")
library("lmtest")

install.packages("bstats")
library("bstats")

bptest(l)
gqtest(wydatki~dochod, fraction=0.33, order.by=~dochod)
white.test(l)

white.test(l2)
bptest(l2)
gqtest(log(wydatki)~log(dochod), fraction=0.33, order.by=~dochod)



# 2

#a
x <- runif(500,1,10)
n <- length(x)

a0 <- 12.5
a1 <- 0.5
eta <- rnorm(600)

eps <- numeric(600)
eps[1] <- eta[1]

for(i in 2:600){
  eps[i] <- eta[i]*sqrt(a0+a1*(eps[i-1])^2)
}
eps

eps <- eps[101:600]
length(eps)

plot(eps,type="l")

b0 <- 5
b1 <- 6

y <- b0+b1*x+eps
plot(y~x)

l <- lm(y~x)
summary(l)



f <- function(a){
  
  a0 <- a[1]
  a1 <- a[2]
  alfa <- a[3]
  beta <- a[4]
  
  m <- numeric(n-1)
  for(t in 2:n){
    m[t-1] <- 1/2*log(a0+a1*(y[t-1]-alfa-beta*x[t-1])^2)+
      (1/2*(y[t]-alfa-beta*x[t])^2)/(a0+a1*(y[t-1]-alfa-beta*x[t-1])^2)
  }
  sum(m)+n/2*log(2*pi)
}


optim(c(12.5,0.5,5,6),f)
optim(c(1,1,4,6),f)
optim(c(1,1,5.4,5.9),f,method="BFGS")  # [1] 12.6609583  0.3723984  5.2215950  5.9194385
optim(c(1,0,5.4,5.9),f)                # [1] 12.6468807  0.3726295  5.2280873  5.9178517

?optim

#b

x <- runif(500,1,2)

rho <- 0.5
eta <- rnorm(600)

eps <- numeric(600)
eps[1] <- eta[1]

for(i in 2:600){
  eps[i] <- rho*eps[i-1]+eta[i]
}
eps

eps <- eps[101:600]
length(eps)

plot(eps,type="l")

b0 <- 5
b1 <- 6

y <- b0+b1*x+eps
plot(y~x)

l <- lm(y~x)
summary(l)


# 3

w <- read.csv2("H://Windows7//Desktop//Ekonometria//2//wig_d.csv",header=TRUE,sep=",") 
head(w)

d <- read.csv2("H://Windows7//Desktop//Ekonometria//2//pzu_d.csv",header=TRUE,sep=",") 
head(d)

n <- nrow(d)
nrow(w)

attach(d)
c <- as.numeric(as.vector(d$Close))

stop_zwr <- numeric(nrow(d)-1)
for(i in 2:length(c)){
  stop_zwr[i] <- log(c[i]/c[i-1])
}
stop_zwr

c2 <- as.numeric(as.vector(w$Close))

stop_zwr_wig <- numeric(nrow(d)-1)
for(i in 2:length(c2)){
  stop_zwr_wig[i] <- log(c2[i]/c2[i-1])
}
stop_zwr_wig

rf <- 0.05/365

y <- stop_zwr-rf
x <- stop_zwr_wig-rf

l <- lm(y~x)   # 0.9091176

install.packages("FinTS")
library("FinTS")
ArchTest(l$residuals)  # nie ma heteroskedastycznosci II rodzaju
white.test(l) # jest hoteroskoeastycznosc I rodzaju

library("stats")
Box.test(l$residuals,type="Ljung-Box")  # jest autokorelacja
dwtest(l,alternative="two.sided")  # jest autokorelacja

library("sandwich")
nw <- NeweyWest(l)

co <- l$coefficients

T1 <- (co[1]-0)/sqrt(nw[1,1])
2*min(pt(T1,n-2),pt(T1,n-2,lower.tail=FALSE))  # tak, intercept jest rowny zero

T2 <- (co[2]-0)/sqrt(nw[2,2])
2*min(pt(T2,n-2),pt(T2,n-2,lower.tail=FALSE))  # beta jest istotny

T3 <- (co[2]-1)/sqrt(nw[2,2])
pt(T3,n-2)  # istotnie mniejsze od jedynki













