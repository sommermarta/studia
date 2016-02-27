
# 7.1

u <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                uscrime.txt",header=TRUE)
head(u)
attach(u)

# a) 

l0 <- lm(R~.-Ex0,data=u)
summary(l0)

# b) 

l1 <- lm(R~Age+Ed+Ex1+U2+W+X,data=u)
summary(l1)

# c)

l2 <- lm(R~Ed+Ex1,data=u)
summary(l2)

# d) oszacowac blad predykcji metoda kroswalidacji typu leave-one-out dla 
#    trzech powyzszych modeli; w ktorym modelu mamy najmniejszy, a w ktorym
#    najwiekszy blad predykcji?

# blad predykcji to: |y_test - x_test * beta|^2

cv <- numeric(ncol(u)-1)

for(i in 1:(ncol(u)-1)){
   m <- lm(R~.-Ex0, data=u, subset=-i)
   xt <- as.matrix(u[i,-c(1,5)])
   cv[i] <- (u[i,"R"] - cbind(1,xt) %*% as.matrix(m$coefficients))^2
}

m0 <- mean(cv)

# kolejne zrobie przy pomocy funkcji predict():

cv1 <- numeric(ncol(u)-1)

for(i in 1:(ncol(u)-1)){
   m <- lm(R~Age+Ed+Ex1+U2+W+X,data=u,subset=-i)
   cv1[i] <- (predict(m,u[i,])-u[i,"R"])^2
}

m1 <- mean(cv1)

cv2 <- numeric(ncol(u)-1)

for(i in 1:(ncol(u)-1)){
   m <- lm(R~Ed+Ex1,data=u,subset=-i)
   cv2[i] <- (predict(m,u[i,])-u[i,"R"])^2
}

m2 <- mean(cv2)

m0; m1; m2

# [1] 846.3996
# [1] 741.9985
# [1] 1183.359

# l1 >= l0 >= l2
# l1 ma najmniejszy blad, czyli l1 to najlepszy model dla tych danych

# 7.2

data(longley)
head(longley)
attach(longley)

# a)

l <- lm(Employed~.,data=longley)
summary(l)

# b) obliczyc macierz korelacji miedzy zmiennymi, wspolczynniki determinacji 
#    wielokrotnej dla zmiennych objasniajacych oraz wspolczynniki podbicia 
#    wariancji (vif) dla zmiennych objasniajacych

pairs(longley)
cor(longley)

x <- as.matrix(longley[,-ncol(longley)])  # wszystko oprocz zmiennej 
                                          # objasnianej
a <- numeric(ncol(x))

for(i in 1:ncol(x)){
  ll <- lm(x[,i]~x[,-i])
  a[i] <- summary(ll)$r.squared
}

a  # [1] 0.9926217 0.9994409 0.9702548 0.7213654 0.9974947 0.9986824

vif <- 1/(1-a) 
vif # [1]  135.53244 1788.51348   33.61889    3.58893  399.15102  758.98060

which(vif<10) # [1] 4

# widac, ze jest bardzo duza zaleznosc w zmiennych (tylko w dla jednej 
# zmiennej vif jest mniejszy niz 10)

# c) dopasowac model regresji grzbietowej; wybrac optymalna wartosc 
#    parametru lambda

library("MASS")
gr <- lm.ridge(Employed~.,data=longley,lambda=seq(0,0.2,0.001))

plot(gr)    # wykres profilowy
abline(h=0)

# pierwsze punkty na wykresach moga nie byc OLS-ami, bo tu mamy dane 
# scentrowane i zestandaryzowane!!!

# jak wybrac lambda?

select(gr)   # to ostatnie to kroswalidacja leave and out -> lambda = 0.003

# modified HKB estimator is 0.004275357 
# modified L-W estimator is 0.03229531 
# smallest value of GCV  at 0.003 

# obliczyc estymatory wspolczynnikow dla lambda rownego 0.03

gr$coef
gr2 <- lm.ridge(Employed~.,data=longley,lambda=0.03)
gr2$coef  

# GNP.deflator        GNP   Unemployed Armed.Forces   Population       Year 
#    0.2200496  0.7693585   -1.1894102   -0.5223393   -0.6861816  4.0064269 

# d) porownac wspolczynnik przy zmiennej GNP dla modelu z punktu a) oraz 
#    dla modelu dopasowanego przy pomocy regrecji grzbietowej z parametrem
#    lambda = 0.03

# beta_gnp_rigde == 0.77
# beta_gnp_ols == -3.582e-02 

detach(longley)

# 7.3

p <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                prostate.data",header=TRUE)
head(p)
attach(p)

# a) dopasowac model liniowy; dokonac eliminacji zmiennych metoda eliminacji
#    wstecz uzywajac kryterium AIC

l <- lm(lpsa~.-train,data=p)
summary(l)

step(l,direction="backward")  
# lpsa ~ lcavol + lweight + age + lbph + svi -> AIC wybral nam takie zmienne

# b) dopasowac model uzywajac metody lasso

install.packages("lars")
library("lars")

lasso <- lars(as.matrix(p[,-c(9,10)]),lpsa,type="lasso")   
# podaje sie najpierw iksy, potem igreki i potem, ze to lasso

plot(lasso,breaks=FALSE)  # wykres profilowy -> lambda rosnie w lewo

lasso$lambda # tu mozna odczytac, ktora to lambda
lasso$beta  # wspolczynniki beta w zaleznosci od lambda

# c) na podstawie lasso i kryterium Cp Mallowsa dokonac selekcji zmiennych;
#    wyswietlic, ktore zmienne sa po kolei dolaczane do modelu, gdzy wartosc
#    parametru lambda rosnie

w <- which(lasso$Cp==min(lasso$Cp))  # wybor lambdy
w   # 6 

lasso$lambda[w-1]                    # bo tam jest indeksowane od zera
# [1] 0.6286376

lasso$beta

# lcavol   lweight          age       lbph        svi        lcp    gleason        pgg45
# 0 0.0000000 0.0000000  0.000000000 0.00000000 0.00000000  0.0000000 0.00000000 0.0000000000
# 1 0.3573072 0.0000000  0.000000000 0.00000000 0.00000000  0.0000000 0.00000000 0.0000000000
# 2 0.3916323 0.0000000  0.000000000 0.00000000 0.09772183  0.0000000 0.00000000 0.0000000000
# 3 0.4729686 0.4010287  0.000000000 0.00000000 0.44189300  0.0000000 0.00000000 0.0000000000
# 4 0.4772307 0.4343405  0.000000000 0.00000000 0.46205106  0.0000000 0.00000000 0.0003752489
# 5 0.4945025 0.4904080  0.000000000 0.03525646 0.55370843  0.0000000 0.00000000 0.0013866469
# 6 0.5067509 0.5431376 -0.008040524 0.06070074 0.58841287  0.0000000 0.00000000 0.0022898666
# 7 0.5115481 0.5748987 -0.012590999 0.07452697 0.61037529  0.0000000 0.01704893 0.0024820450
# 8 0.5643413 0.6220198 -0.021248185 0.09671252 0.76167340 -0.1060509 0.04922793 0.0044575118
# attr(,"scaled:scale")
# [1]  11.548118   4.197556  72.946951  14.214944   4.056305  13.699992   7.075440 276.341974

# czyli patrzymy na 6 lambde -> czyli nieistotne sa lcp i gleason
# dolaczamy po kolei: lcavol, svi, lweight, pgg45, lbph, age
# wybralismy te same zmienne, co AIC :D


# inna funkcja liczaca estymator lasso:

install.packages("glmnet")
library("glmnet")

las <- glmnet(as.matrix(p[,-c(9,10)]),lpsa)
plot(las)

las$beta     # kolumny to lambdy, a wiersze to zmienne
las$lambda   # kolejne lambdy

cv.glmnet(as.matrix(p[,-c(9,10)]),lpsa) 
# na koncu wydruku jest lambda.1se (i to jest ta najlepsza lambda)

# $lambda.min
# [1] 0.03914843
# 
# $lambda.1se
# [1] 0.1903632
# 
# attr(,"class")
# [1] "cv.glmnet"


# kroswalidacja jest losowa, wiec moga wychodzic inne wyniki za kazdym 
# razem!
