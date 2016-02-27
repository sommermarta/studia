# 2.2

install.packages("klaR")
library("klaR")
library("MASS")

k <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/kredit.asc",header=TRUE)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
head(k)
attach(k)

k2 <- k[,1:10]
head(k2)

ktore <- sample(1:length(kredit),500)

tren <- k[ktore,]
test <- k[-ktore,]

# dla wszystkich:

lw <- lda(kredit~.,data=tren)
lw.pred <- predict(lw,newdata=test)
t <- table(test$kredit,lw.pred$class)
proc <- sum(diag(t))/nrow(test)*100
proc   # 76.6

lwkw <- qda(kredit~.,data=tren)
lwkw.pred <- predict(lwkw,newdata=test)
t <- table(test$kredit,lwkw.pred$class)
proc <- sum(diag(t))/nrow(test)*100
proc  # 71.8

# dla 9:

tren9 <- k2[ktore,]
test9 <- k2[-ktore,]

lw9 <- lda(kredit~.,data=tren9)
lw.pred9 <- predict(lw9,newdata=test9)
t <- table(test9$kredit,lw.pred9$class)
proc <- sum(diag(t))/nrow(test9)*100
proc   # 76

lwkw9 <- qda(kredit~.,data=tren9)
lwkw.pred9 <- predict(lwkw9,newdata=test9)
t <- table(test9$kredit,lwkw.pred9$class)
proc <- sum(diag(t))/nrow(test9)*100
proc  # 72.8

# stepclass:

s <- stepclass(tren[,2:ncol(k)],tren[,1],method="lda",direction="backward")
s$model
lws <- lda(s$formula, data=tren)
lws.pred <- predict(lws,newdata=test)
t <- table(test$kredit,lws.pred$class)
proc <- sum(diag(t))/nrow(test)*100
proc   # 69.6

s2 <- stepclass(tren[,2:ncol(k)],tren[,1],method="qda",direction="backward")
s2$model
plot(s2)
lwkws <- qda(s2$formula, data=tren)
lwkws.pred <- predict(lwkws,newdata=test)
t <- table(test$kredit,lwkws.pred$class)
proc <- sum(diag(t))/nrow(test)*100
proc  # 68.4

# 3.3

data(mtcars)
mtcars
attach(mtcars)

mt2 <- mtcars[,1:4]

partimat(as.factor(cyl)~.,data=mt2,method="lda",nplots.vert=2)
partimat(as.factor(cyl)~.,data=mt2,method="qda",nplots.vert=2)


# 3.1

h <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/SAheart.data",sep=",",header=TRUE)
head(h)
attach(h)

# a)

p11 <- length(which(chd==1 & famhist=="Present"))/length(chd)
p00 <- length(which(chd==0 & famhist=="Absent"))/length(chd)
p10 <- length(which(chd==0 & famhist=="Present"))/length(chd)
p01 <- length(which(chd==1 & famhist=="Absent"))/length(chd)

OR <- (p11*p00)/(p10*p01)
OR   # 3.21875  # czyli ewidentnie sa zalezne

# b)

tren <- h[1:324,-1]
test <- h[325:nrow(h),-1]

l <- glm(chd~.,data=tren,family="binomial")  # generalized linear models
summary(l)

l.aic <- step(l,direction="backward",k=2)    # chd ~ tobacco + ldl + famhist + typea + age
l.bic <- step(l,direction="backward",k=log(nrow(tren)))   # chd ~ tobacco + famhist + typea + age

# mozna tez uzyc stepclassa, ale step jest duzo szybszy

# testy, czy model pelny moze byc zastapiony przez model mniejszy:

anova(l.aic,l,test="Chisq")  # mniejszy jest adekwatny
anova(l.bic,l,test="Chisq")  # mniejszy jest adekwatny
anova(l.aic,l.bic,test="Chisq") # aic wychodzi lepsze, choæ ta p-wartosc jest taka na granicy

# c)

summary(l)
exp(l$coefficients[10])  # jesli wiek wzrosnie o rok, to ryzyko zwiekszy sie 1.046 razy - ale nalezy pamietac, ze inne wartosci sa ustalone!

# d) i e)

# czyli mamy takie estymatory do porownania:
# logit
# logit + aic
# logit + bic
# lda
# qda

l_pred <- ifelse(predict(l,newdata=test,type="response")>0.5,1,0) 
t <- table(test$chd,l_pred)
sum(diag(t))/nrow(test)*100   # 77.53

l.aic_pred <- ifelse(predict(l.aic,newdata=test,type="response")>0.5,1,0) 
t <- table(test$chd,l.aic_pred)
sum(diag(t))/nrow(test)*100   # 75.36

l.bic_pred <- ifelse(predict(l.bic,newdata=test,type="response")>0.5,1,0) 
t <- table(test$chd,l.bic_pred)
sum(diag(t))/nrow(test)*100   # 76.81

l.lda <- lda(chd~.,data=tren)
l.lda_pred <- predict(l.lda,newdata=test)
t <- table(test$chd,l.lda_pred$class)
sum(diag(t))/nrow(test)*100  # 76.08

l.qda <- qda(chd~.,data=tren)
l.qda_pred <- predict(l.qda,newdata=test)
t <- table(test$chd,l.qda_pred$class)
sum(diag(t))/nrow(test)*100  # 73.18


# 3.3

e <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/earthquake.txt",header=TRUE) 
head(e)
attach(e)

plot(body,surface,col=c(rep("red",20),rep("blue",79)))

l <- glm(popn~.,data=e,family="binomial")
summary(l)

# nie ma zbieznosci algorytmu :(
# w takich sytuacjach raczej nie warto opierac sie o statystyke walda - otrzymujemy bledny wynik


# 3.4

library("glmnet")
set.seed(20)
load("Leukemia.RData")
x <- Leukemia$x
y <- Leukemia$y

dim(x)   # bardzo duzo predyktorow!

l.logit <- glm(y~x, family="binomial")
summary(l.logit) # wychodzi zupelnie bezsensowny syf

rlas <- glmnet(x,y,family="binomial",alpha=1,lambda.min=1e-4)
plot(rlas)

dim(coef(rlas))
coef(rlas)

rridg <- glmnet(x,y,family="binomial",alpha=0,lambda.min=1e-4)
plot(rridg)
coef(rridg)

cv1 <- cv.glmnet(x,y)  # jak wybrac najlepsze lambda (lambda min to ta, dla ktorej blad kroswalidacji byl najmniejszy)
cv1


