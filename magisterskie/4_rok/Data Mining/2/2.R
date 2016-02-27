# 2.1

library("klaR")
library("MASS")

w <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/
                  wine.data",sep=",")
head(w,3)

# a)

m1 <- lda(V1~V2+V8, data=w)
z1 <- predict(m1,newdata=w)$posterior
head(z1,5)    # pstwo aposteriori

# b)

klasy <- predict(m1,newdata=w)$class
t <- table(w$V1,klasy); t
100*sum(diag(t))/nrow(w)   # procent poprawnych klasyfikacji

# c)

n1 <- length(which(w$V1==1))
n2 <- length(which(w$V1==2))
n3 <- length(which(w$V1==3))

plot(w$V2,w$V8,xlab="alkohol",ylab="flawonidy",
     pch=c(rep("1",n1),rep("2",n2),rep("3",n3)),
     col=c(rep("red",n1),rep("blue",n2),rep("green",n3)))

x <- seq(10.5,15.5,length.out=200)
y <- seq(0,5.5,length.out=200)
siatka <- expand.grid(V2=x,V8=y)
pred <- predict(m1,newdata=siatka)$posterior

z1 <- pred[,1]-pmax(pred[,2],pred[,3])
z2 <- pred[,2]-pmax(pred[,1],pred[,3])
z3 <- pred[,3]-pmax(pred[,2],pred[,1])

contour(x,y,matrix(z1,200),level=0,add=T)
contour(x,y,matrix(z2,200),level=0,add=T)
contour(x,y,matrix(z3,200),level=0,add=T)

# albo inaczej:

partimat(as.factor(V1)~V8+V2,data=w)

# d)

m2 <- qda(V1~V2+V8, data=w)
z1 <- predict(m2,newdata=w)$posterior
head(z1,5)

klasy <- predict(m2,newdata=w)$class
t <- table(w$V1,klasy); t
100*sum(diag(t))/nrow(w)   # wiekszy niz w lda

n1 <- length(which(w$V1==1))
n2 <- length(which(w$V1==2))
n3 <- length(which(w$V1==3))

plot(w$V2,w$V8,xlab="alkohol",ylab="flawonidy",
     pch=c(rep("1",n1),rep("2",n2),rep("3",n3)),
     col=c(rep("red",n1),rep("blue",n2),rep("green",n3)))

x <- seq(10.5,15.5,length.out=200)
y <- seq(0,5.5,length.out=200)
siatka <- expand.grid(V2=x,V8=y)
pred <- predict(m2,newdata=siatka)$posterior

z1 <- pred[,1]-pmax(pred[,2],pred[,3])
z2 <- pred[,2]-pmax(pred[,1],pred[,3])
z3 <- pred[,3]-pmax(pred[,2],pred[,1])

contour(x,y,matrix(z1,200),level=0,add=T)
contour(x,y,matrix(z2,200),level=0,add=T)
contour(x,y,matrix(z3,200),level=0,add=T)

# 2.2

m <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/
                  kredit.asc",header=TRUE)
head(m)

# a)

m_lda <- lda(kredit~.,data=m)
m_qda <- qda(kredit~.,data=m)

t_lda <- table(m$kredit,predict(m_lda,newdata=m)$class); t_lda
100*sum(diag(t_lda))/nrow(m)

t_qda <- table(m$kredit,predict(m_qda,newdata=m)$class); t_qda
100*sum(diag(t_qda))/nrow(m)

# b)

s_lda <- stepclass(m[,2:ncol(m)],m[,1],method="lda",direction="backward")
s_lda$formula

m_lda2 <- lda(s_lda$formula,data=m)
t_lda2 <- table(m$kredit,predict(m_lda2,newdata=m)$class); t_lda2
100*sum(diag(t_lda2))/nrow(m)

s_qda <- stepclass(m[,2:ncol(m)],m[,1],method="qda",direction="backward")
s_qda$formula

m_qda2 <- qda(s_qda$formula,data=m)
t_qda2 <- table(m$kredit,predict(m_qda2,newdata=m)$class); t_qda2
100*sum(diag(t_qda2))/nrow(m)

# 2.3

data(mtcars)
head(mtcars,3)

c <- mtcars[,c(2,1,3,4)]
head(c,3)

partimat(as.factor(c$cyl)~.,data=c,method="lda",nplots.vert=2)
partimat(as.factor(c$cyl)~.,data=c,method="qda",nplots.vert=2)

