

# 5.1

Data <- read.csv2("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/ConcreteData.csv")
head(Data)
names(Data)
dim(Data)

set.seed(1)

n <- nrow(Data)
s <- sample(1:n,500)

#  to jest ok, tylko trzeba tam w lm wpisać nazwę zmiennej, nie żaden skrót, czyli nie żadne c!!!
#  c <- Data$Concrete.compressive.strength.MPa..megapascals.
#  l <- lm(Data$Concrete.compressive.strength.MPa..megapascals.~.,Data)
#  summary(l) 


y <- as.numeric(Data[,9]) 
x <- as.matrix(Data[,1:8])


xx <- x[s,]
yy <- y[s]

l <- lm(yy~xx)
summary(l)


xt <- x[-s,]
yt <- y[-s]


c <- l$coefficients
beta <- as.numeric(c)
mean((yt-cbind(1,xt)%*%beta)^2)

e <- numeric(100)

for(i in 1:100){       #jak losujemy jednš próbkę to słabo, wylosujmy kilka razy i wemy z tego redniš!
   
   s <- sample(1:n,500)
   
   xx <- x[s,]
   yy <- y[s]
   
   l <- lm(yy~xx)
   
   xt <- x[-s,]
   yt <- y[-s]
   
   c <- l$coefficients
   beta <- as.numeric(c)
   e[i] <- mean((yt-cbind(1,xt)%*%beta)^2)
   
}

e
mean(e)

boxplot(e, horizontal=TRUE)

# zmienne x dajemy w kwadracie tak dla zabawy :D

ee <- numeric(100)

for(i in 1:100){       #jak losujemy jednš próbkę to słabo, wylosujmy kilka razy i wemy z tego redniš!
   
   s <- sample(1:n,500)
   
   xx <- (x[s,])^2
   yy <- y[s]
   
   l <- lm(yy~xx)
   
   xt <- (x[-s,])^2
   yt <- y[-s]
   
   c <- l$coefficients
   beta <- as.numeric(c)
   ee[i] <- mean((yt-cbind(1,xt)%*%beta)^2)
   
}
mean(ee)


#a teraz dodamy sobie te zmienne w kwadracie

eee <- numeric(100)

for(i in 1:100){       #jak losujemy jednš próbkę to słabo, wylosujmy kilka razy i wemy z tego redniš!
   
   s <- sample(1:n,500)
   
   xx <- cbind(x[s,],(x[s,])^2)
   yy <- y[s]
   
   l <- lm(yy~xx)
   
   xt <- cbind(x[-s,],(x[-s,])^2)
   yt <- y[-s]
   
   c <- l$coefficients
   beta <- as.numeric(c)
   eee[i] <- mean((yt-cbind(1,xt)%*%beta)^2)
   
}
mean(eee)


boxplot(e,ee,eee,horizontal=TRUE)

#ostatni najlepszy, bo ma najmniejsze błędy!

# 5.5

ch <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/cheese.txt",header=TRUE)
head(ch)

l1 <- lm(ch$taste~ch$Acetic)
l2 <- lm(ch$taste~ch$Acetic+ch$H2S+ch$Lactic,data=ch)

summary(l1)
summary(l2)

#test F - funkcja wbudowana
anova(l1,l2)  #p-value małe, czyli odrzucamy, czyli większy model jest lepszy

# cos poza zadaniem:


# 5.2

s <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/savings.txt",header=T)
head(s)
attach(s)

# a

l <- lm(Savings~.-Country, data=s)
summary(l)
plot(l,1)

rs <- rstudent(l)
rs[abs(rs)>2]

hat <- hatvalues(l)
hat[hat>2*5/length(Savings)]

cook <- cooks.distance(l)
cook[cook==max(cook)]

plot(l,4)   #wykres cooka

ss <- s[-49,]
l2 <- lm(Savings~.-Country, data=ss)
summary(l2)
plot(l2,1)

summary(l)
summary(l2)   #troche sie polepszylo

# b

# dpi jest nieistotna, bo p-value testu t jest duże dla tej zmiennej                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

l1 <- lm(Savings~.-Country-dpi,data=ss)
l2 <- lm(Savings~.-Country-ddpi,data=ss)

l12 <- lm(dpi~.-Country-dpi-Savings,data=ss)
l22 <- lm(ddpi~.-Country-ddpi-Savings,data=ss)

r1 <- l1$residuals
r12 <- l12$residuals
r2 <- l2$residuals
r22 <- l22$residuals

ll <- lm(r1~r12)
lll <- lm(r2~r22)

plot(r1,r12)
abline(ll,col="red")   #prosta pozioma, to jest ona nieistotna (dpi)

plot(r2,r22)
abline(lll,col="blue")   #prosta pochyla, to istotna (ddpi)

# c

cor(Pop15,Pop75)   #widać, że sš silnie zależne

summary(l2,cor=TRUE)  #sla pop15 i 75 jest 0.79

# tam ujemne tu dodatnie
# Intuicyjnie: dwa ujemnie skorelowane predyktory probuja wykonac te sama prace. 
# Im wiecej pracy zrobi jeden, tym mniej moze zrobic drugi i stad dodatnia korelacja wspolczynnikow


# d

# funkcjš wbudowanš, już nie ręcznie

install.packages("faraway")
library("faraway")

summary(l2)  
prplot(l2,1)   # pierwsza, ktora sie wyswietla w summary
# czyli jest istotna
# jaka dodatkowa relacja? rozdzielenie na dwie grupy!!!
# co z tym zrobimy?

#wartosc 35 zostala wybrana na podstawie wykresu
lm(Savings~.-Country, data=ss, subset=Pop15<35)$coef["Pop15"]   # -39
lm(Savings~.-Country, data=ss, subset=Pop15>35)$coef["Pop15"]   # 0.27

#dwie grupy, w jednej zaleznosc jest ujemna, w drugiej dodatnia
#czyli model opierajacy sie na wszystkich danych moze nie byc poprawny

# 5.4

ci <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/cigconsumption.txt",header=T)
head(ci)
attach(ci)

l <- lm(Sales~.-State, data=ci)
summary(l)

# p- value male, czyli jest jakas zaleznosc w danych

# b

l2 <- lm(Sales~.-State-Female-HS, data=ci)
summary(l2)

anova(l,l2)
# duze p-value, czyli mniejszy jest lepszy -> czyli te dwie zmienne mogš być usuniete z modelu

# c

summary(l)    # wiekszy model, wiec wieksze R^2, a R(adj) już się lepiej zachowuje, widać, że mniejszy model jest lepszy
summary(l2)


















































































