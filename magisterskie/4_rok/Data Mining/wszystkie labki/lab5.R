# 5.2

install.packages("mlbench")
library("mlbench")
library("rpart")

data(BreastCancer)
b <- BreastCancer

b <- b[,-1]
head(b)
nrow(b)
table(b$Class)

# + --> chcemy rozpoznawac te zlosliwe

s <- sample(1:nrow(b),350)
tren <- b[s,] 
test <- b[-s,]

nrow(test)
nrow(tren)

tree <- rpart(Class~., data=tren)
plot(tree)
text(tree)

p <- predict(tree,newdata=test)
a <- numeric(nrow(p))
for(i in 1:nrow(p)){
  a[i] <- if(max(p[i,])==p[i,1]) "benign" else "malignant"
}

a
aa <- test$Class

tp <- length(which(a=="malignant" & aa=="malignant"))
tn <- length(which(a=="benign" & aa=="benign"))
fn <- length(which(a=="benign" & aa=="malignant"))
fp <- length(which(a=="malignant" & aa=="benign"))

m <- matrix(c(tp,fp,fn,tn),nrow=2)
m

acc <- (m[1,1]+m[2,2])/nrow(test) 
sens <- m[1,1]/(m[1,1]+m[1,2])
prec <- m[1,1]/(m[1,1]+m[2,1])

acc; sens; prec

acc <- numeric(100)
sens <- numeric(100)
prec <- numeric(100)

for(j in 1:100){
  ss <- sample(1:nrow(b),350)
  tren <- b[ss,] 
  test <- b[-ss,]
  
  tree <- rpart(Class~., data=tren)
  
  p <- predict(tree,newdata=test)
  a <- numeric(nrow(p))
  for(i in 1:nrow(p)){
    a[i] <- if(max(p[i,])==p[i,1]) "benign" else "malignant"
  }
  aa <- test$Class
  
  tp <- length(which(a=="malignant" & aa=="malignant"))
  tn <- length(which(a=="benign" & aa=="benign"))
  fn <- length(which(a=="benign" & aa=="malignant"))
  fp <- length(which(a=="malignant" & aa=="benign"))
  
  tp <- length(which(a=="malignant" & aa=="malignant"))
  tn <- length(which(a=="benign" & aa=="benign"))
  fn <- length(which(a=="benign" & aa=="malignant"))
  fp <- length(which(a=="malignant" & aa=="benign"))
  
  m <- matrix(c(tp,fp,fn,tn),nrow=2)
  
  acc[j] <- (m[1,1]+m[2,2])/nrow(test) 
  sens[j] <- m[1,1]/(m[1,1]+m[1,2])
  prec[j] <- m[1,1]/(m[1,1]+m[2,1])
}

mean(acc); mean(sens); mean(prec)
sd(acc); sd(sens); sd(prec)


# 5.5

library("class")
data(iris)
head(iris)

e <- numeric(100)
eb <- numeric(100)
ebo <- numeric(100)
for(i in 1:100){
  s <- sample(1:nrow(iris),nrow(iris),replace=TRUE)
  
  knn1 <- knn(iris[s,-5],iris[-s,-5],iris[s,5],k=1)
  eb[i] <- length(which(knn1 != iris[-s,5]))/nrow(iris[-s,-5])
  
  knn2 <- knn(iris[s,-5],iris[,-5],iris[s,5],k=1)
  ebo[i] <- length(which(knn2 != iris[,5]))/nrow(iris[,-5])
  
  e[i] <- 0.632*eb[i]+(1-0.632)*ebo[i]
}

mean(e)
mean(eb)
mean(ebo)


# 5.3

# a)

u <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/urine.txt",header=TRUE)
head(u)


siatka <- seq(0,1,length.out=100)
l <- glm(presence~.,data=u,family="binomial")
tpr1 <- numeric(100)
fpr1 <- numeric(100)
k <- 1

for(i in siatka){
 
  l_pred <- ifelse(predict(l,newdata=u,type="response")>i,"yes","no")
  t <- table(u$presence,l_pred)
  if(all(l_pred=="no")) t <- cbind(t,c(0,0))
  if(all(l_pred=="yes")) t <- cbind(c(0,0),t)
  
  tpr1[k] <- t[2,2]/(t[2,2]+t[2,1])
  fpr1[k] <- 1- t[1,1]/(t[1,1]+t[1,2])
  k <- k+1
  
}

plot(fpr1,tpr1,type="s",col="red")
library(ggplot2)

dane
dane <- data.frame(tpr=sort(tpr1), fpr=sort(fpr1))

ggplot(dane, aes(x=fpr, y=tpr))+
  geom_step(size=1)+
  theme_bw()+
  xlab("FPR")+
  ylab("TPR")+
  theme(axis.text = element_text(size=14, family="serif"),
        axis.title = element_text(size=20, family="serif"))

# b)


siatka <- seq(0,1,length.out=100)
l <- glm(presence~mmho+urea+sg,data=u,family="binomial")
tpr2 <- numeric(100)
fpr2 <- numeric(100)
k <- 1

for(i in siatka){
  
  l_pred <- ifelse(predict(l,newdata=u,type="response")>i,"yes","no")
  t <- table(u$presence,l_pred)
  if(all(l_pred=="no")) t <- cbind(t,c(0,0))
  if(all(l_pred=="yes")) t <- cbind(c(0,0),t)
  
  tpr2[k] <- t[2,2]/(t[2,2]+t[2,1])
  fpr2[k] <- 1- t[1,1]/(t[1,1]+t[1,2])
  k <- k+1
  
}

matplot(fpr2,tpr2,type="s",col="blue",add=TRUE)


siatka <- seq(0,1,length.out=100)
tree <- rpart(presence~., data=u)

tpr3 <- numeric(100)
fpr3 <- numeric(100)
k <- 1

for(i in siatka){
  
  tree_pred <- ifelse(predict(tree,newdata=u,type="prob")[,2]>i,"yes","no")
  
  tp <- length(which(tree_pred==u$presence & u$presence=="yes"))
  tn <- length(which(tree_pred==u$presence & u$presence=="no"))
  fn <- length(which(tree_pred!=u$presence & u$presence=="yes"))
  fp <- length(which(tree_pred!=u$presence & u$presence=="no"))
  t <- matrix(c(tn,fn,fp,tp),nrow=2)
  
  tpr3[k] <- t[2,2]/(t[2,2]+t[2,1])
  fpr3[k] <- 1- t[1,1]/(t[1,1]+t[1,2])
  k <- k+1
  
}

matplot(fpr3,tpr3,type="s",col="green",add=TRUE)

lines(c(0.5,0.5),c(-0.1,1.1), lty=4)

# c)

s <- 0
sum <- 0
for(i in 100:2){
  s <- (fpr1[i-1]-fpr1[i])*tpr1[i]
  sum <- sum+s
}
sum

s <- 0
sum <- 0
for(i in 100:2){
  s <- (fpr2[i-1]-fpr2[i])*tpr2[i]
  sum <- sum+s
}
sum

library("MASS")
f <- splinefun(tpr1~fpr1)
area(f,0,1)

#install.packages("ROCR")
library("ROCR")

#?performance

# d)

l <- glm(presence~.,data=u,family="binomial")
l_pred <- predict(l,newdata=u,type="response")
u$presence

a <- order(l_pred,decreasing=TRUE)
aa <- u$presence[a]
aaa <- numeric(length(aa))
for(i in 1:length(aa)){
  if(aa[i]=="yes") aaa[i] <- 1 else aaa[i] <- 0
}
c <- cumsum(aaa)
c <- c(0,c)

plot(c,type="s")  # krzywa LIFT





