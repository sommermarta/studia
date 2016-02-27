# 6.1

pom <- rbinom(200,1,0.9)
y <- numeric(200)

for(i in 1:200){
  if(pom[i]==1) y[i] <- rnorm(1,5,1) else y[i] <- rnorm(1,10,1)
}


yy <- numeric(200)
a <- seq(0,15,leng=200)
for(i in 1:200){
  yy[i] <- 0.9*dnorm(a[i],5,1)+0.1*dnorm(a[i],10,1)
}

hist(y,freq=FALSE,ylim=c(0,0.45))
lines(a,yy,col="green")
lines(density(y),col="red")
legend("topright",col=c("black","green","red"),c("histogram","prawdziwa","jadrowa"),lty=1)

# blad:

d <- density(y)

x <- seq(2,12,length.out=512)
yyy <- numeric(length(x))
for(i in 1:length(x)){
  yyy[i] <- 0.9*dnorm(x[i],5,1)+0.1*dnorm(x[i],10,1)
}

yyyy <- numeric(length(d$x))
for(i in 1:length(d$x)){
  yyyy[i] <- 0.9*dnorm(d$x[i],5,1)+0.1*dnorm(d$x[i],10,1)
}

f <- splinefun(d$x,d$y)

sum((yyy-f(x))^2)/length(yyy)   # blad dla rownoodleglych
sum((yyyy-d$y)^2)/length(yyyy)  # blad dla tych automatycznych

# jak blad zalezy od h

sh <- seq(0.05,1.5,length.out=100)
bl <- numeric(length(sh))
for(j in 1:100){
  d <- density(y,bw=sh[j])
  yyyy <- numeric(length(d$x))
  for(i in 1:length(d$x)){
    yyyy[i] <- 0.9*dnorm(d$x[i],5,1)+0.1*dnorm(d$x[i],10,1)
  }
  bl[j] <- sum((yyyy-d$y)^2)/length(yyyy)
}

plot(sh,bl,lty=1,type="l")

# 6.2

library("MASS")

data(geyser)
head(geyser)

x <- geyser$waiting
y <- geyser$duration

h1 <- bw.SJ(x)
h2 <- bw.SJ(y)

f.SJ <- kde2d(x,y,n=100,h=c(h1,h2))
persp(f.SJ,col="orange",phi=10,theta=-50,xlab="waiting",ylab="duration",zlab="density")

# zbyt poszarpany, wiec zwiekszmy h:

h11 <- 4*bw.SJ(x)
h22 <- 4*bw.SJ(y)

f.SJ <- kde2d(x,y,n=100,h=c(h11,h22))
persp(f.SJ,col="orange",phi=10,theta=-50,xlab="waiting",ylab="duration",zlab="density")

# 6.3

e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/earthquake.txt",header=TRUE)
head(e)

bq <- e$body[which(e$popn=="equake")]
bx <- e$body[which(e$popn=="explosn")]

d1 <- density(bq,bw=0.2)
d2 <- density(bx,bw=0.2)

plot(d2,col="red",type="l",xlim=c(4,7))
lines(d1,col="green")















