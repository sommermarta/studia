#4.1

r <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/realest.txt",header=TRUE)
head(r)
dim(r)



l <- lm(r$Price~., data=r)
summary(l)       #widac, ze bedroom istotna, no i room i garage tez (kropka)

# p-value od F jest male, wiec ktores zmienne beda istotne

# a
# cena spada - troche nielogiczne, dlaczego?
# bo reszta ustalona!!!!

l2 <- lm(r$Price~r$Bedroom)
summary(l2)  # tu juz jest dodatni

co <- summary(l)$coefficients

co[1,1] + 0*co[9,1] + 3*co[2,1] + 1500*co[3,1] + 8*co[4,1] + 40*co[5,1] + 2*co[7,1] + 1*co[8,1] + 1000*co[6,1]
predict(l,newdata=data.frame(Bedroom=3,Space=1500,Room=8,Lot=40,Bathroom=2,Garage=1,Tax=1000,Condition=0),interval="prediction")  #zrobi sie tu jeszcze przedzial ufnosci


#4.2

g <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/gala_data.txt",header=TRUE)
s <- g$Species
e <- g$Endemics
attach(g)

#a

l <- lm(s~.-e,data=g)   #wszystkie oprócz endemics   #cos tu nie dziala bez attacha
summary(l)

l <- lm(Species~.-Endemics,data=g)
summary(l)
plot(l,1,which=1)

#b

l2 <- lm(sqrt(Species)~.-Endemics,data=g)
summary(l2)
plot(l2,1)

#c

# nearest ma najwieksza p value
l3 <- lm(Species~.-Endemics-Nearest,data=g)
summary(l3)
plot(l3,1)

summary(l);summary(l3)   #w drugim modelu wieksze R^2 adj, a zwyk³y siê nie ró¿ni


# 5.1

Data <- read.csv2("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/ConcreteData.csv")
head(Data)
names(Data)
dim(Data)

set.seed(1)

n <- nrow(Data)
s <- sample(1:n,500)

#  to jest ok, tylko trzeba tam w lm wpisaæ nazwê zmiennej, nie ¿aden skrót, czyli nie ¿adne c!!!
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

for(i in 1:100){       #jak losujemy jedn¹ próbkê to s³abo, wylosujmy kilka razy i weŸmy z tego œredni¹!
  
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

for(i in 1:100){       #jak losujemy jedn¹ próbkê to s³abo, wylosujmy kilka razy i weŸmy z tego œredni¹!
  
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

for(i in 1:100){       #jak losujemy jedn¹ próbkê to s³abo, wylosujmy kilka razy i weŸmy z tego œredni¹!
  
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

#ostatni najlepszy, bo ma najmniejsze b³êdy!

# 5.5

ch <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/cheese.txt",header=TRUE)
head(ch)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
l1 <- lm(ch$taste~ch$Acetic)
l2 <- lm(ch$taste~ch$Acetic+ch$H2S+ch$Lactic,data=ch)

summary(l1)
summary(l2)

#test F - funkcja wbudowana
anova(l1,l2)  #p-value ma³e, czyli odrzucamy, czyli wiêkszy model jest lepszy

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

# dpi jest nieistotna, bo p-value testu t jest du¿e dla tej zmiennej                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

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

cor(Pop15,Pop75)   #widaæ, ¿e s¹ silnie zale¿ne

summary(l2,cor=TRUE)  #sla pop15 i 75 jest 0.79

# tam ujemne tu dodatnie
# Intuicyjnie: dwa ujemnie skorelowane predyktory probuja wykonac te sama prace. 
# Im wiecej pracy zrobi jeden, tym mniej moze zrobic drugi i stad dodatnia korelacja wspolczynnikow


# d

# funkcj¹ wbudowan¹, ju¿ nie rêcznie

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
 # duze p-value, czyli mniejszy jest lepszy -> czyli te dwie zmienne mog¹ byæ usuniete z modelu

# c

summary(l)    # wiekszy model, wiec wieksze R^2, a R(adj) ju¿ siê lepiej zachowuje, widaæ, ¿e mniejszy model jest lepszy
summary(l2)


# 6.1

cr <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/uscrime.txt",header=T)
head(cr)
attach(cr)

# a)

l <- lm(R~., data=cr)
summary(l)

pairs(cr)   #miedzy ex i ex0 jest liniowa zaleznosc!!!
cor(cr)    #tu tez to widac -> 0.9935 miedzy ex0 i ex1

# jesli zmienne sa silnie skorelowane, to usuwamy jedn¹ z nich

l2 <- lm(R~.-Ex0,data=cr)
summary(l2)
# teraz widac, ¿e zmienna ex1 jednak jest istotna, co bylo wczesniej zamaskowane przez te wspolliniowosc

# b) 

extractAIC(l2,k=2)   #AIC
# wartosc funkcji kryterialnej, to ta druga -> 13 to liczba zmiennych w modelu

extractAIC(l2,k=log(nrow(cr)))   #BIC

# musielibysmy sprawdzac wszystkie podmodele!!! i wybrac podmodele

2^13
# no dobra, zróbmy to:

combn(1:4,2)   #wszystkie podzbiory dwuelementowe zbioru {1,2,3,4}


#x <- as.matrix(cr[,-c(1,5)])  ???????????????
x <- as.matrix(cr[,-1])
y <- as.matrix(cr[,1])
s <- numeric(2^13)
to <- extractAIC(l2,k=2)[2]
nr <- extractAIC(l2,k=2)[1]

for(i in 1:13){
    
    com <- combn(1:13,i)
    
    for(j in 1:ncol(com)){
      model <- com[,j]
      mod <- lm(y~c[,model])
      ac <- extractAIC(mod,k=2)[2]
      if(ac<to){
        to <- ac
        nr <- model
      } 
    }  
}


to
nr
head(x)

# zwykle tak sie nie robi :D
# zamiast tego stosuje sie procedury zachlanne 

# c)

l3 <- lm(R~.-Ex0, data=cr, subset=-c(11,19))  #outliery usuwamy
l3_b <- step(l3,direction="backward")

l3_null <- lm(R~1,data=cr,subset=-c(11,19))
l3_f <- step(l3_null,direction="forward",scope=list(lower=~1,upper=l3)) #zaczynamy od modelu z interceptem, a konczymy na modelu pelnym
# dla metody forward trzeba zdefiniowac pusty model l3_null
# aic jest wszedzie domyslny

l3_oba <- step(l3,direction="both",k=log(nrow(cr)-2))   # minus dwa, zeby nie liczyl outlierow, ktore wyrzucilismy wczesneij

# d)

summary(l3)
#zaimplementowax to sobie :D
# najszybsza procedura i dzia³a calkiem dobrze 

# 6.3

library("car")
?linearHypothesis

data(Davis)
attach(Davis)
head(Davis)
?Davis

mod.davis <- lm(weight~repwt,data=Davis)
summary(mod.davis)
# h0: b0=0, b1=1, czyli zaleznosc jest scisle liniowa wtedy
linearHypothesis(mod.davis,diag(2),c(0,1))   #sprawdza nam tê hipotezê -> (F spore) przyjmujemy -> ludzie nie naginaja prawdy :D
# inny zapis:
linearHypothesis(mod.davis,c("(Intercept)=0","repwt=1"))
linearHypothesis(mod.davis,c("(Intercept)","repwt"),c(0,1))

data(Duncan)
attach(Duncan)
head(Duncan)

mod.duncan <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan)   # widac, ze parametry podobne

linearHypothesis(mod.duncan,c(0,1,-1),0)
linearHypothesis(mod.duncan,"income=education") #duze F-value, to przyjmujemy hipotezê (patrz zeszyt, jaka hipoteza)

# b

chol <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/ExerciseCholesterol.txt",header=TRUE)
chol
# chol1 <- chol[which(chol$Group==1),]
# chol2 <- chol[which(chol$Group==2),]
# chol3 <- chol[which(chol$Group==3),]

attach(chol)

install.packages("Matrix")
library("Matrix")

# patrz zeszyt!!!

x <- chol$Weight
x
X <- as.matrix(bdiag(list(cbind(1,x[1:8]),cbind(1,x[9:16]),cbind(1,x[17:26]))))
X

C <- matrix(c(0,0,1,0,0,0,-1,1,0,0,0,-1),nrow=2)
C
d <- matrix(c(0,0),ncol=1)
d

l <- lm(chol$HDL~X+0)  #zeby bylo bez interceptu # F wieksze niz 0.05, wiec nie ma roznic w tych grupach (ale tak naprawdê jesteœmy na granicy troszkê :D)
summary(l)

linearHypothesis(l,C,d)
















































































