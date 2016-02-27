# 6.1

cr <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/
                 DANE/uscrime.txt",header=T)
head(cr)
attach(cr)

# a)

l <- lm(R~., data=cr)
summary(l)

pairs(cr)   # rysuje wykresy rozproszenia dla kazdej pary zmiennych 
            # widac, ze miedzy Ex1 i Ex0 jest liniowa zaleznosc
cor(cr)     # tu tez to widac -> 0.9935 miedzy Ex0 i Ex1

# jesli zmienne sa silnie skorelowane, to usuwamy jedn¹ z nich

l2 <- lm(R~.-Ex0,data=cr)
summary(l2)
# teraz widac, ¿e zmienna Ex1 jednak jest istotna, co bylo wczesniej 
# zamaskowane przez te wspolliniowosc

# b) 

# AIC
extractAIC(l2,k=2)  # [1]  13.0000 302.8369 (liczba zmiennych w modelu i 
                    #                        wartosc funkcji kryterialnej)

# BIC
extractAIC(l2,k=log(nrow(cr)))   # [1]  13.0000 326.8888

# Musimy obliczyc AIC i BIC dla kazdego modelu i sprawdzic, dla ktorego
# wartosc funkcji kryterialnej jest minimalna. Ten model bedzie najlepszy.
# No wiec:

x <- as.matrix(cr[,-1])
y <- as.matrix(cr[,1])
s <- numeric(2^13)
to <- extractAIC(l2,k=2)[2]
nr <- extractAIC(l2,k=2)[1]

for(i in 1:13){
   
   com <- combn(1:13,i)
   
   for(j in 1:ncol(com)){
      model <- com[,j]
      mod <- lm(y~x[,model])
      ac <- extractAIC(mod,k=2)[2]
      if(ac<to){
         to <- ac
         nr <- model
      } 
   }  
}


to   # [1] 291.8293
nr   # [1]  1  3  4 11 12 13
head(x) # czyli model bedzie sie skladal z: Age, Ed, Ex0, U2, W, X

# zwykle tak sie nie robi :D
# zamiast tego stosuje sie procedury zachlanne 

step(l2,direction="backward")

l_null <- lm(R~1,data=cr)
step(l_null,direction="forward",scope=list(lower=~1,upper=l2)) 

# zaczynamy od modelu z interceptem, a konczymy na modelu pelnym
# dla metody forward trzeba zdefiniowac pusty model l_null
# AIC jest wszedzie domyslny

step(l2,direction="both") 

# tu wybralo taki model:
# Step:  AIC=295.53
# R ~ Age + Ed + Ex1 + M + U1 + U2 + W + X

# 6.3

library("car")

data(Davis)
attach(Davis)
head(Davis)

# badamy zaleznosc realnej wagi ludzi od wagi przez nich zadeklarowanej 

mod.davis <- lm(weight~repwt,data=Davis)
summary(mod.davis)

# H0: b_0 = 0, b_1 = 1, czyli badamy, czy y = x1 (czyli czy ludzie mowia
#                                                 prawde co do swojej wagi)
# y = b_0 + b_1*x_1 + eps_i
# Cb = d
# b = (b_0,b_1)
# C = (1 0)
#     (0 1)
# d = (0 1)

linearHypothesis(mod.davis,diag(2),c(0,1)) 

# p-value = 0.1793 -> duze, to przyjmujemy hipoteze, czyli ludzie mowia 
#                     prawde

data(Duncan)
attach(Duncan)
head(Duncan)

mod.duncan <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan)  

# badamy, czy wplyw education i income jest taki sam
# y = b0 + b1*x1 + b2*x2 + eps_i
# H0: b1 = b2
# b = (b0,b1,b2)
# C = (0,1,-1)
# d = 0

linearHypothesis(mod.duncan,c(0,1,-1),0)

# p-value = 0.7952 - duze, wiec przyjmujemy hipoteze, czyli b1 = b2

# b)

chol <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR
                   /DANE/ExerciseCholesterol.txt",header=TRUE)
chol
attach(chol)

install.packages("Matrix")
library("Matrix")

x <- chol$Weight
X <- as.matrix(bdiag(list(cbind(1,x[1:8]),cbind(1,x[9:16]),
                          cbind(1,x[17:26]))))

C <- matrix(c(0,0,1,0,0,0,-1,1,0,0,0,-1),nrow=2)
d <- matrix(c(0,0),ncol=1)

l <- lm(chol$HDL~X+0)  # zeby bylo bez Interceptu 
                       
summary(l)

linearHypothesis(l,C,d)

# p-value = 0.07542 -> duze, wiec przyjmujemy hipoteze, wiec nie ma 
#                      roznic w grupach



























