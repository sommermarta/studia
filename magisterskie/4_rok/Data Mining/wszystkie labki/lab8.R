# 8.1

library("rpart")

k <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/kredit.asc",header=TRUE)
head(k)

# a)

ile <- floor(2*nrow(k)/3)
s <- sample(1:1000,ile)
tren <- k[s,]
test <- k[-s,]  

tree <- rpart(as.factor(kredit)~.,data=tren)
plot(tree)
text(tree)
p <- predict(tree,newdata=test, type="class")

t <- table(test[,1],p)
100*sum(diag(t))/sum(t)

# b)

ile <- floor(2*nrow(k)/3)
s <- sample(1:1000,ile)
tren <- k[s,]
test <- k[-s,] 

# dla 25:

a <- matrix(0,nrow=25,ncol=334)
for(i in 1:25){
  sss <- sample(1:ile,ile,replace=TRUE)
  tren2 <- tren[sss,]
  
  tree <- rpart(as.factor(kredit)~.,data=tren2)
  p <- predict(tree,newdata=test, type="class")  
  a[i,] <- ifelse(p=="1",1,0)
}

aa <- apply(a,2,sum)
pr <- ifelse(aa>0.5*25,1,0)
t <- table(test[,1],pr)
t
100*sum(diag(t))/sum(t)

# dla 100:

a <- matrix(0,nrow=100,ncol=334)
for(i in 1:100){
  sss <- sample(1:ile,ile,replace=TRUE)
  tren2 <- tren[sss,]
  
  tree <- rpart(as.factor(kredit)~.,data=tren2)
  p <- predict(tree,newdata=test, type="class")  
  a[i,] <- ifelse(p=="1",1,0)
}

aa <- apply(a,2,sum)
pr <- ifelse(aa>0.5*100,1,0)
t <- table(test[,1],pr)
t
100*sum(diag(t))/sum(t)

# teraz jeszcze dodatkowo usrednie sobie po zbiorze treningowym i testowym

ile <- floor(2*nrow(k)/3)
proc25 <- numeric(50)
proc100 <- numeric(50)

for(m in 1:50){
  
  s <- sample(1:1000,ile)
  tren <- k[s,]
  test <- k[-s,] 
  
  a <- matrix(0,nrow=25,ncol=334)
  for(i in 1:25){
    sss <- sample(1:ile,ile,replace=TRUE)
    tren2 <- tren[sss,]
    
    tree <- rpart(as.factor(kredit)~.,data=tren2)
    p <- predict(tree,newdata=test, type="class")  
    a[i,] <- ifelse(p=="1",1,0)
  }
  
  aa <- apply(a,2,sum)
  pr <- ifelse(aa>0.5*25,1,0)
  t <- table(test[,1],pr)
  proc25[m] <- 100*sum(diag(t))/sum(t)
  
  a <- matrix(0,nrow=100,ncol=334)
  for(i in 1:100){
    sss <- sample(1:ile,ile,replace=TRUE)
    tren2 <- tren[sss,]
    
    tree <- rpart(as.factor(kredit)~.,data=tren2)
    p <- predict(tree,newdata=test, type="class")  
    a[i,] <- ifelse(p=="1",1,0)
  }
  
  aa <- apply(a,2,sum)
  pr <- ifelse(aa>0.5*100,1,0)
  t <- table(test[,1],pr)
  proc100[m] <- 100*sum(diag(t))/sum(t)
  
}

mean(proc25)
mean(proc100)

# jest do tego biblioteka:
# library(adabag)
# bagging i boosting mozemy tam znalezc

library("adabag")
library("randomForest")

# 8.3

g <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/agaricus-lepiota.data", sep=",")
head(g)

s <- sample(1:nrow(g),nrow(g)/2)
tren <- g[s,]
test <- g[-s,]

las <- randomForest(V1~.,data=tren,xtest=test[,2:23],ytest=test[,1])
las

varImpPlot(las)   # pokazuje ktore zmienne sa istotne -> te co na gorze, jest najbardziej istotna (V6)




















