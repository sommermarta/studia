# zad.1

sz <- read.table("http://gamma.mini.pw.edu.pl/~szymanowskih/lab5/szereg.txt")
sz <- as.numeric(sz[,1])
head(sz)
ts.plot(sz)
n <- length(sz)

# pierwszy sposob - roznicowanie:

szd <- diff(sz)   # zmienia sie rzad szeregu MA!!!
ts.plot(szd)

mod1 <- arima(szd,c(1,0,3),method="ML",include.mean=TRUE)

phi1 <- mod1$coef[1]
theta1 <- mod1$coef[2]+1
theta2 <- -mod1$coef[4]
a <- mod1$coef[5]

# b - tak mozemy je odzyskac:

ut <- sz-(1:n)*a
b <- mean(ut)

par <- c(phi1,theta1,theta2,a,b)
names(par) <- c("phi1","theta1","theta2","a","b")
par

# drugi sposob - dopasowanie modelu:

l <- lm(sz~c(1:n))
summary(l)

ut <- sz - l$coef[1]-l$coef[2]*(1:n)

mod2 <- arima(ut,c(1,0,2),method="ML",include.mean=FALSE) 
# dlaczego bez sredniej? Bo byla juz uwzgledniona wczesniej!

par2 <- c(mod2$coef,l$coef[2:1])
names(par2) <- c("phi1","theta1","theta2","a","b")

par
par2    # oprocz interceptu wyszlo podobnie :D

# prawdziwa wartosc: c(-3/4,-1/3,1/2,5)  -> tak byly generowane te dane
# wyraz wolny ogolnie sie bardzo slabo estymuje...

# zad.2

lac <- read.table("http://gamma.mini.pw.edu.pl/~szymanowskih/lab5/LACounty.txt",header=TRUE)
head(lac,2)

# a)

plot(lac$TEMP,lac$CARDIO) 
# widac, ze w miare ukladaja sie na paraboli, 
# wiec wprowadzmy nowa zmienna kwadrawowa:

t <- lac$DATE
y <- lac$CARDIO
t1 <- lac$TEMP - mean(lac$TEMP)
t2 <- (lac$TEMP - mean(lac$TEMP))^2
p <- lac$PART

# b)

mod1 <- lm(y~t+t1+t2+p)
summary(mod1)

# c)

r1 <- mod1$residuals
Box.test(r1,lag=20,type="Ljung")  # reszty nie sa bialym szumem

# d)

modr1 <- ar(r1)
modr1$ar

# e)
# model zmodyfikowany:

coeff <- c(1,-modr1$ar)

library("quantmod")

for (name in c("t","y","t1","t2","p")) {
  v <- get(name)   # wartosc zmiennej, ktora ma taka nazwe, jak ten string
  assign(paste("N",name,sep=""),cbind(v,Lag(v,1),Lag(v,2),Lag(v,3))%*%coeff)  
  # assingn("x",5) to to samo co x <- 5  
}

mod2 <- lm(Ny~Nt+Nt1+Nt2+Np)
summary(mod2)

r2 <- mod2$residuals
plot(r2,type="l")
Box.test(r2,lag=20,type="Ljung")   
# poprawilo sie, ale dalej jest nie najlepiej

# co proponujemy w takiej sytuacji? jeszcze raz to samo!

# f)

modr2 <- ar(r2)
modr2$ar  # rzad dwa

coeff <- c(1,-modr2$ar)

for (name in c("Nt","Ny","Nt1","Nt2","Np")) {
  v <- get(name)   
  assign(paste("N",name,sep=""),cbind(v,Lag(v,1),Lag(v,2))%*%coeff)    
}

mod3 <- lm(NNy~NNt+NNt1+NNt2+NNp)
summary(mod3)

r3 <- mod3$residuals
Box.test(r3,lag=20,type="Ljung")  # no prawie :D

# z tych danych juz wiecej nie wycisniemy -> to sa dane rzeczywiste, 
# dlatego tak opornie idzie

modr3 <- ar(r3)
modr3$ar  # nic nie zwraca, wiec nie dopasujemy juz dalej

acf(r3)  # acf nie sa takie zle w sumie, wystaje akurat ten 19, 
         # wiec box test o niego zahacza
pacf(r3) # podobnie

# z kazda iteracja cos tam uzyskiwalismy, wiec metoda dziala :D 

