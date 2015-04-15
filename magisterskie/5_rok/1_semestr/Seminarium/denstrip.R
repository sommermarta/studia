library("denstrip")
library("survival")

n <- rnorm(200)
e <- rexp(200)
m <- c(rnorm(100,0,1),rnorm(100,4,1))

# bpstrip ---> Box-percentile strips

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
bpstrip(n,at=0.5,width=0.5)
bpstrip(e,at=1.5,width=0.5)
bpstrip(m,at=3,width=0.5)

# cistrip

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
cistrip(c(mean(n),min(n),max(n)), at=0.5, horiz=T)
cistrip(c(mean(e),min(e),max(e)), at=1.5, horiz=T)
cistrip(c(mean(m),min(m),max(m)), at=3, horiz=T)

# denstrip

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
denstrip(n,at=0.5,ticks=mean(n))
denstrip(e,at=1.5,ticks=mean(e))
denstrip(m,at=3,ticks=mean(m),colmax="blue")  # mo¿na te¿ daæ to w kolorze
denstrip(m,at=3,ticks=mean(m),colmax="yellow")  # mo¿na te¿ daæ to w kolorze, slabo...

denstrip.legend(-4,2.5,width=0.3,len=1.5)  # dodawanie legendy

# denstrip.normal

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
denstrip.normal(0,2,at=0.5)
denstrip.normal(0,2,at=1.5,gamma=0.2)   # zabawa z gamma
denstrip.normal(0,2,at=3,gamma=5)

# sectioned.density

plot.new()
plot.window(c(-5,6),c(0,4))
axis(side=1)
sectioned.density(n,at=0.5)
sectioned.density(e,at=1.5)
sectioned.density(m,at=3)

# vwstrip

plot.new()
plot.window(c(-5,7),c(0,4))
axis(side=1)
vwstrip(n,at=0.5,width=0.5)
vwstrip(e,at=1.5,width=0.5)
vwstrip(m,at=3,width=0.5)

# vwstrip.normal

plot.new()
plot.window(c(-5,7),c(0,4))
axis(side=1)
vwstrip.normal(0,1,at=0.5,width=0.5)
vwstrip.normal(2,0.5,at=1.5,width=0.5)
vwstrip.normal(2,2,at=3,width=0.5)

#####################################

fit <- survfit(Surv(time, status) ~ 1, data=aml, conf.type="log-log")
plot(fit,col=c("black","red","red"),
     main="Kaplan-Meier estimate of survival",
     ylab="Survival",xlab="Time")
plot(fit, col=0,main="Kaplan-Meier estimate of survival",
     ylab="Survival",xlab="Time")
densregion(fit)
lines(fit, lwd=2, conf.int=FALSE, lty=1)
lines(fit, lwd=1, conf.int=TRUE, lty=2, col=c("black","red","red"))


