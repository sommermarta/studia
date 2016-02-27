#zad1
?distribution
dnorm
pnorm
curve(pnorm(x,0,1),from=-5,to=5,col="red",main="dystrybuanta rozk³aku normalnego",xlab="x",ylab="F(x)")
curve(pnorm(x,1,1),from=-5,to=5,col="green",add=T)
curve(pnorm(x,2,1),from=-5,to=5,col="blue",add=T)

curve(dnorm(x,0,1),from=-5,to=5,col="red",main="dystrybuanta rozk³aku normalnego",xlab="x",ylab="F(x)")
curve(dnorm(x,1,1),from=-5,to=5,col="green",add=T)
curve(dnorm(x,2,1),from=-5,to=5,col="blue",add=T)

x<-seq(-3,3,by=0.01)
plot(x,dnorm(x),type="l",lwd=4)

#zad2
#P(u-3b<=x<=u+3b)=~0,997

pnorm(3)-pnorm(-3)
x<-seq(-4,4,by=0.01)
y<-dnorm(x)
plot(x,y,type="l")
polygon(c(-3,x[x>=-3 & x<=3],3),c(0,y[x>=-3 & x<=3],0),col="yellow")
arrows(-3,0.025,3,0.025,code=3,length=0.1)
text(0,0.05,expression(paste(mu,"=/-3",sigma)))
text(0,0.15,paste(round(100*(pnorm(3)-pnorm(-3)),2),"%",sep=" "))

#zad3
#X~N(173,6)
curve(dnorm(x,173,6),from=140,to=210)
abline(v=173)

#P(x<=179)
pnorm(179,173,6)
#P(x in [167,180])
pnorm(180,173,6)-pnorm(167,173,6)
#P(x>181)=1-P(x<181)
1-pnorm(181,173,6)
#P(X<x)=0,6
qnorm(0.6,173,6)


#zad4
?distributions
#a
qf(0.99,3,18)


#zad10
?dbinom
r1<-dbinom(0:10,10,0.5)
barplot(r1,names=0:10)

#zad15
n<-1000
v<-runif(n)
u<-runif(n)
plot(u,v,xlim=c(0,1),ylim=c(0,1))
curve(x*x,col="red",type="l",xlim=c(0,1),add=T,lwd=3)

z<-(v<u*u)
sum(z)/n
kwadrat<-function(x) x^2
integrate(kwadrat,0,1)


#zadanie w zaciszu domowym











