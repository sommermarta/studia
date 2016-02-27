# zad 1.1

e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE
                /earthquake.txt",header=TRUE)
e
attach(e)

# a)

plot(surface~body,data=e[which(e$popn=="equake"),],col="red",
     ylim=c(min(e$surface),max(e$surface)),
     xlim=c(min(e$body),max(e$body)),pch="q")
points(e[which(e$popn=="explosn"),2],e[which(e$popn=="explosn"),3],
       pch="x",col="blue")

# b)

sq <- cov(e[which(e$popn=="equake"),2:3])
sx <- cov(e[which(e$popn=="explosn"),2:3])
sq; sx

n <- length(e$popn)
nq <- length(which(e$popn=="equake"))
nx <- length(which(e$popn=="explosn"))
n; nq; nx

W <- ((nq-1)*sq + (nx-1)*sx)/(n-2) 
W

x1 <- c(mean(e$body[which(e$popn=="equake")]),
        mean(e$surface[which(e$popn=="equake")]))
x2 <- c(mean(e$body[which(e$popn=="explosn")]),
        mean(e$surface[which(e$popn=="explosn")]))
x1; x2

a <- solve(W)%*%(x1-x2)
a

sr <- (x1+x2)/2

wsp_a <- -a[1]/a[2]
intercept <- t(a)%*%sr / a[2]

abline(a=intercept,b=wsp_a)

detach(e)











































































