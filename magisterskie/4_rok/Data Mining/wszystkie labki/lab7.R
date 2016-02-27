# 7.1

# a)

library("e1071")

data1 <- seq(1,10,by=2)
classes1 <- c("a","a","a","b","b")
test1 <- seq(1,10,by=2) + 1

mod <- svm(data1,classes1,type="C",kernel="linear")
pr <- predict(mod,data1)
table(pr,classes1)

pr2 <- predict(mod,test1)
table(pr2,classes1)

# b)

data2 <- seq(1,10)
classes2 <- c("b","b","b","a","a","a","a","b","b","b")

modb <- svm(data2,classes2,type="C",kernel="linear")
pr <- predict(modb,data2)
table(pr,classes2)   # zle dziala

modb2 <- svm(data2,classes2,type="C",kernel="radial")
pr <- predict(modb2,data2)
table(pr,classes2)  # stosujac odpowiednia funkcje jadrowa ladnie dziala


# 7.2

library("MASS")
data(cats)
head(cats)  # waga w [kg] i waga serca w [g]

m1 <- svm(Sex~.,data=cats,type="C",kernel="radial",gamma=0.1)
plot(m1,cats,svSymbol="")

m1 <- svm(Sex~.,data=cats,type="C",kernel="radial",gamma=1)
plot(m1,cats,svSymbol="")

m1 <- svm(Sex~.,data=cats,type="C",kernel="radial",gamma=5)
plot(m1,cats,svSymbol="")   # im wieksza gamma, tym bardziej dopasowany do danych

# c)

m2 <- svm(Sex~.,data=cats,type="C",kernel="polynomial",degree=1)
plot(m2,cats,svSymbol="")   # powinna byc linia prosta

m2 <- svm(Sex~.,data=cats,type="C",kernel="polynomial",degree=3)
plot(m2,cats,svSymbol="")   # powinna byc linia prosta

m2 <- svm(Sex~.,data=cats,type="C",kernel="polynomial",degree=5)
plot(m2,cats,svSymbol="")   # powinna byc linia prosta

# wada svm -> dwa parametry do wyboru :( 
# robi sie to kroswalidacja:

data(iris)

obj <- tune(svm, Species~., data = iris, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "cros")
)

summary(obj)
plot(obj)

# 7.3

d <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/breast-cancer-wisconsin.data",sep=",",header=TRUE)
head(d)
d <- d[,-1]
head(d)

tren <- d[1:400,]
test <- d[401:nrow(d),]

# a)

m1 <- svm(Class~.,data=tren, type="C")
p1 <- predict(m1,test[,-10])
t1 <- table(p1,test$Class)
t1
proc1 <- 100*(sum(diag(t1))/sum(t1))
proc1
czul1 <- t1[2,2]/sum(t1[2,])
czul1
prec1 <- t1[2,2]/sum(t1[,2])
prec1

obj <- tune(svm, Class~., data = tren, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(1:4)),
            tunecontrol = tune.control(sampling = "cross")
)
summary(obj)
plot(obj)             # im ciemniejsze, tym mniejszy blad i te parametry trzeba wybierac

m2 <- svm(Class~.,data=tren,type="C",gamma=1,cost=2)
p2 <- predict(m2,test[,-10])
t2 <- table(p2,test$Class)
t2
proc2 <- 100*(sum(diag(t2))/sum(t2))
proc2
czul2 <- t2[2,2]/sum(t2[2,])
czul2
prec2 <- t2[2,2]/sum(t2[,2])
prec2


