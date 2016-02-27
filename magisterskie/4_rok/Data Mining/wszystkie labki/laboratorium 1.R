library("MASS")
library("lattice")

# 1.1

e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM
                /DANE/earthquake.txt",header=TRUE)
head(e)
attach(e)

# a)

n <- nrow(e)
ile <- table(popn); ile
nq <- ile[1]
nx <- ile[2]

kol <- c(rep("red",nq),rep("blue",nx))
styl <- c(rep("Q",nq),rep("X",nx))

plot(body,surface,type="n")
points(body,surface,col=kol,pch=styl,cex=0.5)

# b)

# macierze kowariancji w klasach:

eq <- e[popn=="equake",]
ex <- e[popn=="explosn",]

cov_q <- cov(eq[,2:3]); cov_q
cov_x <- cov(ex[,2:3]); cov_x

# macierz kowariancji wewnatrzgrupowej:

w <- ((nq-1)*cov_q + (nx-1)*cov_x)/(n-2); w

# pierwszy wektor kanoniczny:

sr_q <- apply(eq[,2:3],2,mean); sr_q
sr_x <- apply(ex[,2:3],2,mean); sr_x

a <- solve(w)%*%t(t(sr_x - sr_q)); a

# prosta rozdzielajaca klasy:

z <- 1/2*(sr_x + sr_q)

wsp_kier <- -a[1]/a[2]
wyr_woln <- t(a)%*%t(t(z)) / a[2]

abline(wyr_woln,wsp_kier)

# c)

l <- lda(popn~.,data=e)
l$scaling    # pierwszy wektor kanoniczny (wyszedl troche inny niz 
             # liczylismy recznie, ale wystarczy by byl proporcjonalny)

# d)

x_siat <- seq(4.5,6.5,length.out=50)
y_siat <- seq(3.5,6.5,length.out=50)
siatka <-  expand.grid(body=x_siat,surface=y_siat)

l_pred <- predict(l,siatka)$class

kol2 <- ifelse(l_pred == "equake","green","yellow")
styl2 <- ifelse(l_pred == "equake", 19, 22)
plot(siatka, pch=styl2, col=kol2)                 
text(e$body, e$surface, styl, col=kol, cex=0.5)

# e)

zm_kanon <- t(a) %*% t(e[,2:3])
prog <- t(a)%*%z
    
pr <- ifelse(as.numeric(zm_kanon)-prog > 0,"explosn","equake")
pr == as.character(e$popn)  # widac, ze tylko pierwsza zle sklasyfikowana 

# tabela reklasyfikacji

pred <- predict(l,newdata=e)$class
table(popn,pred)

# f)

x0 <- as.data.frame(t(c(body=6,surface=4)))
predict(l,x0)$class

# 1.3

w <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM
                /DANE/wine.data", sep=",")
head(w)
attach(w)

# a)

now <- w[,c(1,2,8)]
n1 <- length(which(now[,1]==1))
n2 <- length(which(now[,1]==2))
n3 <- length(which(now[,1]==3))
n <- nrow(now)

kol <- c(rep("red",n1),rep("blue",n2),rep("green",n3))
styl <- c(rep("1",n1),rep("2",n2),rep("3",n3))

plot(now[,2],now[,3],col=kol,pch=styl,cex=0.6)

# b)

s <- sample(1:n,n/2)
tren <- w[s,]
test <- w[-s,]

l <- lda(V1~V2+V8, data=tren)
l_pred <- predict(l,newdata=test)$class
t <- table(test$V1,l_pred)

# procent poprawnej klasyfikacji:

sum(diag(t))/nrow(test)*100

# c)

x_siat <- seq(11,15.5,length.out=50)
y_siat <- seq(0,5.5,length.out=50)
siatka <-  expand.grid(V2=x_siat,V8=y_siat)

l_pred <- predict(l,siatka)$class

kol2 <- ifelse(l_pred == "1","yellow",ifelse(l_pred=="2","orange","white"))
styl2 <- ifelse(l_pred == "1",19,ifelse(l_pred=="2",22,24))
plot(siatka, pch=styl2, col=kol2)                 
points(now[,2],now[,3],col=kol,pch=styl,cex=0.6)

# d)

l3 <- lda(V1~V2+V8+V14,data=tren)
l3_pred <- predict(l3,newdata=test)$class
t <- table(test$V1,l3_pred)

sum(diag(t))/nrow(test)*100   # wiecej niz w poprzednim modelu

# e)

kol <- c(rep("red",n1),rep("blue",n2),rep("green",n3))

cloud(w$V14 ~ w$V8*w$V2, screen=c(z=-10,x=0,y=20),
      xlab="Alcohol",ylab="Flavonoids",zlab="Protoline",
      col=kol, pch=c(rep(1,n1), rep(20,n2), rep(24,n3)))

# 1.4

# a)

Y1 <- c(rep(1,n1),rep(0,n2+n3))
Y2 <- c(rep(0,n1),rep(1,n2),rep(0,n3))
Y3 <- c(rep(0,n1+n2),rep(1,n3))

w.lm1 <- lm(Y1 ~ V2+V8, data=w)
w.lm2 <- lm(Y2 ~ V2+V8, data=w)
w.lm3 <- lm(Y3 ~ V2+V8, data=w)

# b)

p1 <- predict(w.lm1,newdata=w)
p2 <- predict(w.lm2,newdata=w)
p3 <- predict(w.lm3,newdata=w)

razem <- cbind(p1,p2,p3)

p <- numeric(n)
for(i in 1:n){
   p[i] <- which(razem[i,] == max(razem[i,]))
}

(t <- table(w$V1,p))
sum(diag(t))/n*100




