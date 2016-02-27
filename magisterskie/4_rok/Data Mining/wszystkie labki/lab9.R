# data mining - zadanie domowe nr 4 - MARTA SOMMER BSMAD

library("KernSmooth")

# stworzenie modelu:

n <- 500
x <- runif(n,min=0,max=4)
eps <- rnorm(n,0,0.1)

f1 <- function(x){
  
  4.26*(exp(-x)-4*exp(-2*x)+3*exp(-3*x))

}

f2 <- function(x){sin(3*x)}

f3 <- function(x){
  (x>0 & x<=1)*x + (x>1 & x<=2)*(-x+2) + (x>2 & x<=3)*(x-2) + (x>3 & x<=4)*(-x+4)
}

y1 <- f1(x)+eps
y2 <- f2(x)+eps
y3 <- f3(x)+eps

# dopasowanie krzywych roznymi metodami: 

l1 <- locpoly(x,y1,bandwidth=0.25)
l2 <- locpoly(x,y2,bandwidth=0.25)
l3 <- locpoly(x,y3,bandwidth=0.25)

k1 <- ksmooth(x,y1,kernel="normal")
k2 <- ksmooth(x,y2,kernel="normal")
k3 <- ksmooth(x,y3,kernel="normal")

s1 <- smooth.spline(x,y1,spar=0.75)
s2 <- smooth.spline(x,y2,spar=0.75)
s3 <- smooth.spline(x,y3,spar=0.75)

lo1 <- loess(y1~x,span=0.5)
lo2 <- loess(y2~x,span=0.5)
lo3 <- loess(y3~x,span=0.5)

plo1 <- predict(lo1,data.frame(x=seq(0,4,by=0.01)))
plo2 <- predict(lo2,data.frame(x=seq(0,4,by=0.01)))
plo3 <- predict(lo3,data.frame(x=seq(0,4,by=0.01)))

# wykresy:

curve(f1,from=0,to=4,col="red")
matplot(x,y1,type="p",pch=1,add=TRUE)
matplot(l1$x,l1$y,type="l",col="green",add=TRUE)
matplot(k1$x,k1$y,type="l",col="orange",add=TRUE)
matplot(s1$x,s1$y,type="l",col="blue",add=TRUE)
matplot(seq(0,4,by=0.01),plo1,type="l",col="violet",add=TRUE)
legend("bottomright",col=c("red","black","green","orange","blue","violet"),
       c("prawdziwa","punkty","locpoly","ksmooth","smooth.spline","loess"),
       lty=c(1,0,1,1,1,1), pch=c(NA,1,NA,NA,NA,NA))

curve(f2,from=0,to=4,col="red")
matplot(x,y2,type="p",pch=1,add=TRUE)
matplot(l2$x,l2$y,type="l",col="green",add=TRUE)
matplot(k2$x,k2$y,type="l",col="orange",add=TRUE)
matplot(s2$x,s2$y,type="l",col="blue",add=TRUE)
matplot(seq(0,4,by=0.01),plo2,type="l",col="violet",add=TRUE)
legend("topright",col=c("red","black","green","orange","blue","violet"),
       c("prawdziwa","punkty","locpoly","ksmooth","smooth.spline","loess"),
       lty=c(1,0,1,1,1,1), pch=c(NA,1,NA,NA,NA,NA))
  
curve(f3,from=0,to=4,col="red")
matplot(x,y3,type="p",pch=1,add=TRUE)
matplot(l3$x,l3$y,type="l",col="green",add=TRUE)
matplot(k3$x,k3$y,type="l",col="orange",add=TRUE)
matplot(s3$x,s3$y,type="l",col="blue",add=TRUE)
matplot(seq(0,4,by=0.01),plo3,type="l",col="violet",add=TRUE)
legend("topright",col=c("red","black","green","orange","blue","violet"),
       c("prawdziwa","punkty","locpoly","ksmooth","smooth.spline","loess"),
       lty=c(1,0,1,1,1,1), pch=c(NA,1,NA,NA,NA,NA))

# ISE:

ise_l1 <- sum((f1(l1$x)-l1$y)^2)/length(l1$x)
ise_l2 <- sum((f2(l2$x)-l2$y)^2)/length(l2$x)
ise_l3 <- sum((f3(l3$x)-l3$y)^2)/length(l3$x)

ise_l1; ise_l2; ise_l3


ise_k1 <- sum((f1(k1$x)-k1$y)^2)/length(k1$x)
ise_k2 <- sum((f2(k2$x)-k2$y)^2)/length(k2$x)
ise_k3 <- sum((f3(k3$x)-k3$y)^2)/length(k3$x)

ise_k1; ise_k2; ise_k3


ise_s1 <- sum((f1(s1$x)-s1$y)^2)/length(s1$x)
ise_s2 <- sum((f2(s2$x)-s2$y)^2)/length(s2$x)
ise_s3 <- sum((f3(s3$x)-s3$y)^2)/length(s3$x)

ise_s1; ise_s2; ise_s3

w1 <- which(is.na(plo1)==TRUE)
w2 <- which(is.na(plo2)==TRUE)
w3 <- which(is.na(plo3)==TRUE)

ise_lo1 <- sum((f1(seq(0,4,by=0.01)[-w1])-plo1[-w1])^2)/length(plo1[-w1])
ise_lo2 <- sum((f2(seq(0,4,by=0.01)[-w2])-plo2[-w2])^2)/length(plo2[-w2])
ise_lo3 <- sum((f3(seq(0,4,by=0.01)[-w3])-plo3[-w3])^2)/length(plo3[-w3])

ise_lo1; ise_lo2; ise_lo3

# zaleznosc miedzy bledem ise i liczba obserwacji:


















