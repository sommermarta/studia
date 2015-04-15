# slajd 1

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      ylab="Probability",lwd=2,col="blue")
lines(c(0,0),c(-1,1),lty=2)

curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      ylab="Probability",lwd=2,col="red")

f <- function(x){
   0.5*dnorm(x)+0.5*dnorm(x,4,1)
}
curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylab="Probability",lwd=2, col="black")
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)

curve(dnorm,-3,6,main="Densities of different distributions", 
      ylab="Probability",lwd=2, col="blue",ylim=c(-0.1,0.9))
curve(dexp,0.0001,6,lwd=2,col="red",add=TRUE)
curve(f,-5,9,lwd=2, col="black",add=TRUE)
lines(c(0,0),c(-1,1),lty=2)
legend("topright",c("N(0,1)","Exp(1)","N(0,1)+N(4,1)"),
       col=c("red","blue","black"),lty=1,cex=0.75)

# slajd 2

n <- rnorm(200)
e <- rexp(200)
m <- c(rnorm(200,0,1),rnorm(200,4,1))

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,0.6), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
axis(1)
cistrip(c(mean(n),-max(-min(n),max(n)),max(-min(n),max(n))), 
        at=0.5, horiz=T,lwd=2,col="blue")


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.1,1.5),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
cistrip(c(mean(e),min(e),max(e)), at=1.3, horiz=T,lwd=2,col="red")


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.4),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
cistrip(c(mean(m),min(m),max(m)), at=0.3, horiz=T,,lwd=2)
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
cistrip(c(mean(n),-max(-min(n),max(n)),max(-min(n),max(n))), 
        at=0.5, horiz=T,col="blue",lwd=2)
cistrip(c(mean(e),min(e),max(e)), at=1.5, horiz=T,col="red",lwd=2)
cistrip(c(mean(m),min(m),max(m)), at=3, horiz=T,lwd=2)
title(main="Densities of different distributions")

# slajd 3

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,1.1), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
boxplot(n,horizontal=TRUE,at=0.8,add=TRUE,col="blue",outline=FALSE,
        cex=1,frame=FALSE)


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.1,1.5),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
boxplot(e,horizontal=TRUE,add=TRUE, at=1.3,col="red",
        frame=FALSE,outline=FALSE)


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.8),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
boxplot(m,horizontal=TRUE,add=TRUE,at=0.6,col="gray",
        frame=FALSE,outline=FALSE)
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,7),c(0,4))
axis(side=1)
boxplot(n,horizontal=TRUE,at=0.5,add=TRUE,col="blue",frame=FALSE,outline=FALSE)
boxplot(e,horizontal=TRUE,add=TRUE, at=2,col="red",frame=FALSE,outline=FALSE)
boxplot(m,horizontal=TRUE,add=TRUE,at=3.5,col="gray",frame=FALSE,outline=FALSE)
title(main="Densities of different distributions")

# slajd 4

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,1.1), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
axis(1)
bpstrip(n,at=0.8,width=0.5,col="blue")


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.1,1.5),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
bpstrip(e,at=1.3,width=0.5,col="red")


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.7),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
bpstrip(m,at=0.5,width=0.35)
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
bpstrip(n,at=0.5,width=0.5, col="blue")
bpstrip(e,at=1.5,width=0.5,col="red")
bpstrip(m,at=3,width=0.5)
title(main="Densities of different distributions")

# slajd 5

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,1.1), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
axis(1)
vwstrip(n,at=0.8,width=0.3,col="blue")


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.5,1.5),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
vwstrip(e,at=1.2,width=0.3,col="red")


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.7),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
vwstrip(m,at=0.5,width=0.3)
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
vwstrip(n,at=0.5,width=0.5,col="blue")
vwstrip(e,at=1.5,width=0.5,col="red")
vwstrip(m,at=3,width=0.5)
title(main="Densities of different distributions")

# slajd 6

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,1.1), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
axis(1)
sectioned.density(n,at=0.5,colmax="blue")


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.5,1.7),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
sectioned.density(e,at=1.2,colmax="red")


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.7),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
sectioned.density(m,at=0.5)
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
sectioned.density(n,at=0.5,colmax="blue")
sectioned.density(e,at=1.5,colmax="red")
sectioned.density(m,at=3)
title(main="Densities of different distributions")

# slajd 7

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,0.6), ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
axis(1)
denstrip(n,at=0.5,ticks=mean(n),colmax="blue")


curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.1,1.5),
      axes=FALSE)
lines(c(0,0),c(-1,2),lty=2)
axis(1)
denstrip(e,at=1.3,ticks=mean(e),colmax="red")


curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,0.4),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
lines(c(0,0),c(-1,1),lty=2)
lines(c(4,4),c(-1,1),lty=2)
axis(1)
denstrip(m,at=0.3,ticks=mean(m))
lines(c(2,2),c(-1,1),lty=2)

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
denstrip(n,at=0.5,ticks=mean(n),colmax="blue")
denstrip(e,at=1.5,ticks=mean(e),colmax="red")
denstrip(m,at=3,ticks=mean(m))
title(main="Densities of different distributions")
denstrip.legend(-4,2,width=0.3,len=1.5)

# slajd 8

curve(dnorm,-5,5,main="Density of N(0,1) distribution", 
      lwd=2,col="blue",ylim=c(-0.1,5), ylab="", xlab="",
      axes=FALSE)
axis(1)
cistrip(c(mean(n),-max(-min(n),max(n)),max(-min(n),max(n))), 
        at=0.7, horiz=T,col="blue",lwd=2)
boxplot(n,horizontal=TRUE,add=TRUE, at=1.1,col="blue",
        frame=FALSE,outline=FALSE)
vwstrip(n,at=1.7,width=0.3,col="blue")
bpstrip(n,at=2.2,width=0.5,col="blue")
sectioned.density(n,at=2.7,colmax="blue")
denstrip(n,at=4,ticks=mean(n),colmax="blue")
lines(c(0,0),c(0,5),lty=3)

curve(dexp,0.0001,5,main="Density of Exp(1) distribution", 
      lwd=2,col="red", ylab="", xlab="", ylim=c(-0.1,6),
      axes=FALSE)
axis(1)
cistrip(c(mean(e),min(e),max(e)), at=1.4, horiz=T,lwd=2,col="red")
boxplot(e,horizontal=TRUE,add=TRUE, at=2,col="red",
        frame=FALSE,outline=FALSE)
vwstrip(e,at=2.6,width=0.3,col="red")
bpstrip(n,at=3.3,width=0.5,col="red")
sectioned.density(n,at=3.9,colmax="red")
denstrip(n,at=5.8,ticks=mean(n),colmax="red")
lines(c(0,0),c(0,10),lty=3)

curve(f,-5,9,
      main="Mixture of N(0,1) and N(4,1) distributions", 
      ylim=c(-0.1,6),lwd=2, col="black", ylab="", xlab="",
      axes=FALSE)
cistrip(c(mean(m),min(m),max(m)), at=1, horiz=T,lwd=2)
boxplot(m,horizontal=TRUE,add=TRUE, at=2,col="gray",
        frame=FALSE,outline=FALSE)
vwstrip(m,at=2.6,width=0.3)
bpstrip(m,at=3.3,width=0.5)
sectioned.density(m,at=3.9)
denstrip(m,at=5.8,ticks=mean(m))
lines(c(0,0),c(-1,10),lty=2)
lines(c(4,4),c(-1,10),lty=2)
axis(1)
lines(c(2,2),c(-1,10),lty=2)


