# 9.1

library("pls")
library("faraway")
data(meatspec)
attach(meatspec)
head(meatspec)

rmse <- function(x,y){
   sqrt(mean((x-y)^2))
}

# a)

m <- plsr(fat~.,data=meatspec[1:172,],scale=TRUE)
summary(m)
pr <- predict(m,newdata=meatspec[173:215,])
rmse(pr,meatspec$fat[173:215])   # [1] 4.075829

# b)

m2 <- lm(fat~.,data=meatspec[1:172,])
cm2 <- m2$coefficients
cm <- coefficients(m)

plot(cm2,col="red")
points(cm,col="blue")
abline(h=0)

# widoczny efekt sciagania

# c)

summary(m) # wybieramy 14 pierwszych

#      1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X      98.50    99.23    99.86    99.99   100.00   100.00   100.00   100.00
# fat    22.69    62.92    82.82    89.71    94.18    94.62    94.95    95.51
#      9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X     100.00    100.00    100.00    100.00    100.00    100.00    100.00
# fat    95.81     96.14     96.26     97.21     97.33     97.62     97.88
#      16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X      100.00    100.00     100.0    100.00    100.00    100.00    100.00
# fat     98.03     98.11      98.3     98.44     98.62     98.69     98.81
#      23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
# X      100.00    100.00    100.00    100.00    100.00    100.00    100.00
# fat     99.02     99.09     99.17     99.21     99.26     99.31     99.37
#      30 comps  31 comps  32 comps  33 comps  34 comps  35 comps  36 comps
# X       100.0    100.00    100.00    100.00     100.0    100.00    100.00
# fat      99.4     99.44     99.47     99.49      99.5     99.52     99.53
#      37 comps  38 comps  39 comps  40 comps  41 comps  42 comps  43 comps
# X      100.00    100.00    100.00     100.0    100.00    100.00    100.00
# fat     99.55     99.57     99.59      99.6     99.61     99.62     99.63
#      44 comps  45 comps  46 comps  47 comps  48 comps  49 comps  50 comps
# X      100.00    100.00    100.00    100.00    100.00    100.00    100.00
# fat     99.64     99.65     99.66     99.66     99.67     99.67     99.67
#      51 comps  52 comps  53 comps  54 comps  55 comps  56 comps  57 comps
# X      100.00    100.00    100.00    100.00    100.00    100.00    100.00
# fat     99.68     99.68     99.68     99.68     99.69     99.69     99.69
#      58 comps  59 comps  60 comps  61 comps  62 comps  63 comps  64 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      65 comps  66 comps  67 comps  68 comps  69 comps  70 comps  71 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      72 comps  73 comps  74 comps  75 comps  76 comps  77 comps  78 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      79 comps  80 comps  81 comps  82 comps  83 comps  84 comps  85 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      86 comps  87 comps  88 comps  89 comps  90 comps  91 comps  92 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      93 comps  94 comps  95 comps  96 comps  97 comps  98 comps  99 comps
# X       100.0     100.0     100.0     100.0     100.0     100.0     100.0
# fat      99.7      99.7      99.7      99.7      99.7      99.7      99.7
#      100 comps
# X        100.0
# fat       99.7

pred <- predict(m,newdata=meatspec[173:215,],ncomp=14)
rmse(pred,meatspec$fat[173:215])  # [1] 2.001386

# d)

round(as.matrix(m$loadings[,1]),digits=4) 

# pierwsza skladowa to suma wszystkich zmiennych*0.1 

plot(m$loadings[,2],type="l",col="blue")
lines(1:100,m$loadings[,1],col="red")
abline(h=0)

# 9.2

data(yarn)
head(yarn)

# tylko 28 obserwacji, a duzo zmiennych (MNK by tu nie zadzialalo...)

# a) 

yarn.pcr <- pcr(density~NIR, data=yarn[yarn$train,], validation="CV")
summary(yarn.pcr)
pred <- predict(yarn.pcr, newdata = yarn[!yarn$train,])
rmse2 <- sqrt(mean((pred - yarn$density[!yarn$train])^2)) # [1] 2.817551

# b)

yarn.pls <- plsr(density ~ NIR, data = yarn[yarn$train,], validation = "CV")
summary(yarn.pls)
pred <- predict(yarn.pls, comps = 1:13, newdata = yarn[!yarn$train,])
rmse(pred,yarn$density[!yarn$train])  # [1] 0.09504633

yarn.pls <- plsr(density ~ NIR, data = yarn[yarn$train,], validation = "LOO")
summary(yarn.pls)
pred <- predict(yarn.pls, comps = 1:14, newdata = yarn[!yarn$train,])
rmse(pred,yarn$density[!yarn$train])  # [1] 0.0964903

# c)

plot(yarn.pls, "loadings" ,comps=1:2)
abline(h=0)

# wizualizacja wektorow a1 i a2
# koncowe zmienne nie sa wlasciwie brane pod uwage, nie sa istotne (od 150 i dalej)

# 9.3 

p <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                prostate.data",header=TRUE)
head(p)

# a)

m1 <- lm(lpsa~.-train,data=p[p$train,])
summary(m1)
pr <- predict(m1,p[!p$train,])
rmse(pr,p$lpsa[!p$train])  # [1] 0.7219931

a <- step(m1,direction="backward",k=log(length(which(p$train))))
m2 <- lm(a,data=p[p$train,])
pr <- predict(m2,p[!p$train,])
rmse(pr,p$lpsa[!p$train])   # [1] 0.7017709

# b)

m3 <- pcr(lpsa~.-train,data=p[p$train,],validation="CV")
summary(m3)
pr <- predict(m3,comps=1:8,newdata=p[!p$train,])
rmse(pr,p$lpsa[!p$train])   # [1] 0.7219931

# c)

m4 <- plsr(lpsa~.-train,data=p[p$train,],validation="CV")
summary(m4)
pr <- predict(m4,comps=1:4,newdata=p[!p$train,])
rmse(pr,p$lpsa[!p$train])   # [1] 0.726199


