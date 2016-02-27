# zad 1.1

e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/earthquake.txt",header=TRUE)
e
attach(e)

# a)

plot(surface~body,data=e[which(e$popn=="equake"),],col="red",
     ylim=c(min(e$surface),max(e$surface)),
     xlim=c(min(e$body),max(e$body)),pch="q")
points(e[which(e$popn=="explosn"),2],e[which(e$popn=="explosn"),3],pch="x",col="blue")

# b)

e1 <- e[which(e$popn=="equake"),]
e2 <- e[which(e$popn=="explosn"),]
sr1b <- mean(e1$body)
sr1s <- mean(e1$surface)

sr2b <- mean(e2$body)
sr2s <- mean(e2$surface)

sr1 <- c(sr1b,sr1s)
sr2 <- c(sr2b,sr2s)

s1 <- cov(e[popn=="equake",2:3])
s2 <- cov(e[popn=="explosn",2:3])

w <- 1/(dim(e)[1]-2)*(19*s1+8*s2)

a <- solve(w)%*%(sr1-sr2)
a
to <- (1/2)*(t(t(sr2))+t(t(sr1)))

aa <- -a[1]/a[2]
aa
bb <- as.numeric(t(a)%*%to / a[2])
bb

aa; bb
abline(bb,aa)

# c)

library("MASS")

ld <- lda(popn~.,data=e)
names(ld)

ld$scaling

a

#e

kanon <- t(a)%*%t(as.matrix(e[,2:3]))
kanon

#prog:
prog <- 0.5*t(a)%*%(sr1+sr2)

which(as.numeric(kanon)>as.numeric(prog))

# tabela

ld.pred <- predict(ld,newdata=e)
table(e$popn,ld.pred$class)  #pierwszy argument rzeczywiste etykiety, drugi to przewidywane

#f

# z rysunku odczytalismy, ¿e to explosn
# mo¿na podstawiæ do wzoru na prost¹ rozdzielaj¹c¹ i sprawdziæ, czy jest to wiêksze, czy mniejsze ni¿ zero



e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/wine.data",sep=",")
e
attach(e)

#a

plot(V2,ylim=c(min(e$V2),min(e$V8)),xlim=c(m(e$V8),max(e$V8)))

points(V8,col="red")

plot(V2,V8,col=c(rep("red",59),rep("blue",71),rep("black",48)))

#b

ld <- lda(V1~V2+V8,data=e)
ld$scaling

ldpred <- predict(ld,newdata=e)
t <- table(V1,ldpred$class)

proc <- 1-14/178

procinaczej <- sum(diag(t))/nrow(e)*100
procinaczej

#d

ld3 <- lda(V1~V2+V8+V14,data=e)
ldpred3 <- predict(ld3,newdata=e)
t3 <- table(V1,ldpred3$class)
procinaczej3 <- sum(diag(t3))/nrow(e)*100

#dla wszystkich zmiennych
ldw <- lda(V1~.,data=e)
ldpredw <- predict(ldw,newdata=e)
tw <- table(V1,ldpredw$class)
procinaczejw <- sum(diag(tw))/nrow(e)*100

# dla zbioru testowego

e
s <- sample(1:nrow(e),nrow(e)/2)

etren <- e[s,]
etest <- e[-s,]

ld <- lda(V1~V2+V8,data=etren)
ldpred <- predict(ld,newdata=etest)
t <- table(etest$V1,ldpred$class)
procinaczej <- sum(diag(t))/nrow(etren)*100
procinaczej

#d

ld3 <- lda(V1~V2+V8+V14,data=eten)
ldpred3 <- predict(ld3,newdata=etest)
t3 <- table(etest$V1,ldpred3$class)
procinaczej3 <- sum(diag(t3))/nrow(etren)*100
procinaczej3

#dla wszystkich zmiennych
ldw <- lda(V1~.,data=etren)
ldpredw <- predict(ldw,newdata=etest)
tw <- table(etest$V1,ldpredw$class)
procinaczejw <- sum(diag(tw))/nrow(etren)*100
procinaczejw

# cos a la kroswalidacja

a <-  numeric(100)
b <-  numeric(100)
c <-  numeric(100)

for(i in 1:100){
  s <- sample(1:nrow(e),floor(2*nrow(e)/3))
  
  etren <- e[s,]
  etest <- e[-s,]
  
  ld <- lda(V1~V2+V8,data=etren)
  ldpred <- predict(ld,newdata=etest)
  t <- table(etest$V1,ldpred$class)
  procinaczej <- sum(diag(t))/sum(t)*100
  a[i] <- procinaczej
  
  ld3 <- lda(V1~V2+V8+V14,data=etren)
  ldpred3 <- predict(ld3,newdata=etest)
  t3 <- table(etest$V1,ldpred3$class)
  procinaczej3 <- sum(diag(t3))/nrow(etest)*100
  b[i] <- procinaczej3
  
  ldw <- lda(V1~.,data=etren)
  ldpredw <- predict(ldw,newdata=etest)
  tw <- table(etest$V1,ldpredw$class)
  procinaczejw <- sum(diag(tw))/nrow(etest)*100
  c[i] <- procinaczejw
  
}

mean(a);mean(b);mean(c)



#e

library("lattice")

cloud(V1~V2*V8*V14,col=c(rep("red",59),rep("blue",71),rep("black",48)))

?cloud
















