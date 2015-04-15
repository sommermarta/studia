library("MASS")
library("rpart")
library("car")
library("faraway")
library("lattice")
library("lme4")
library("dplyr")
library("acepack")
library("mgcv")
library("lmtest")

setwd("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych")

######################## Zad.1.1 ###############################

conc <- 0:4
dead <- c(2,8,15,23,27)
number <- rep(30,5)

bliss <- data.frame(conc, dead, number)
bliss

alive <- number-dead
dane <- data.frame(dead, alive)
dane <- as.matrix(dane)
dane

model <- glm(dane~conc, family=binomial(link=logit), data=bliss)
summary(model)

exp(coef(model)[2]) # tyle razy zwiekszy sie szansa zgonu, przy wzroscie
                    # conc o 1

######################## Zad.1.2 ###############################

n1 <- 25
y1 <- 10

n2 <- 50
y2 <- 20

# a)

logwiar <- function(y, n, p){
   choose(n,y) + y*log(p) + (n-y)*log(1-p)
}

p <- seq(from=0, to=1, by=0.01)
ile <- length(p)

wyk1 <- numeric(ile)
wyk2 <- numeric(ile)

for(i in 1:ile){
   wyk1[i] <- logwiar(y1, n1, p[i])
   wyk2[i] <- logwiar(y2, n2, p[i])
}

plot(p, wyk1-wyk1[which(p==y1/n1)], type="l", col="blue")
lines(p, wyk2-wyk2[which(p==y2/n2)], col="red")

# dla wiekszej probki, jest wieksza krzywizna, czyli informacja jest lepsza
# -> informacja Fishera

# b)

fun1 <- function(x){
   -(10*log(x)+15*log(1-x)+log(choose(25,10)))
}

fun2 <- function(x){
   -(20*log(x)+30*log(1-x)+log(choose(50,20)))
}

nlm(fun1,0.2)
nlm(fun1,0.3)
nlm(fun1,0.5)
nlm(fun1,0.6)
nlm(fun1,0.8)

nlm(fun2,0.2)
nlm(fun2,0.3)
nlm(fun2,0.5)
nlm(fun2,0.6)
nlm(fun2,0.8)

# c)

k1 <- nlm(fun1,0.3,hessian=TRUE)
k2 <- nlm(fun1,0.7,hessian=TRUE)

k3 <- nlm(fun2,0.3,hessian=TRUE)
k4 <- nlm(fun2,0.7,hessian=TRUE)

k1 
k3  # widac, ze w fun2 jest dwa razy wiecej informacji

k2
k4

######################## Zad.1.4 ###############################

conc <- 0:4
dead <- c(2,8,15,23,27)
number <- rep(30,5)

conc2 <- rep(conc, each=30)
dead2 <- numeric(30*5)
for(i in 1:5){
   dead2[(30*(i-1)+1):(30*i)] <- c(rep(1,dead[i]),rep(0,30-dead[i]))
}

bliss2 <- data.frame(conc2, dead2)
bliss2

model <- glm(dead2~conc2, data=bliss2, family="binomial")
summary(model)

# wyszło to samo :D

######################## Zad.2.1 ###############################

y <- c(rep(0,4),rep(1,4))
x <- c(seq(10, 40, by=10),seq(60,90, by=10))
dane <- data.frame(y,x)
dane
model <- glm(y~x, data=dane, family="binomial")
summary(model)

# problem, bo dane są idealnie separowalne

######################## Zad.2.2 ###############################

b1 <- 0.3
b2 <- 1
x <- runif(10)
pi <- exp(b1+b2*x)/(1+exp(b1+b2*x))
dane <- matrix(0,10,10)

for(i in 1:10){
   dane[i,] <- rbinom(10,1,pi[i]) 
}

dane

y <- apply(dane, 1, sum)

igreki <- cbind(sukcesy=y,porazki=10-y)
igreki

model <- glm(igreki~pi, family="binomial")
summary(model)

# test dopasowania modelu:

1-pchisq(model$deviance, model$df.residual)

# h0: dobrze dopasowany
# ha: zle dopasowany

# czyli nasz model jest dobrze dopasowany do danych

# procent deviancji wyjasnianej przez model (pseudo R^2):

(1 - model$deviance / model$null.deviance)*100

# wykresy rezyduow:

r_d <- residuals(model, type='deviance')
r_p <- residuals(model , type='pearson')
r_resp <- residuals(model, type='response')

plot(r_d)
plot(r_d, predict(model, type='link'))
plot(r_d, predict(model, type='response'))
prplot(model,1)

# b)

pi <- exp(b1+b2*x)/(1+exp(b1+b2*x))+rnorm(10,0,0.01)
dane <- matrix(0,10,10)

for(i in 1:10){
   dane[i,] <- rbinom(10,1,pi[i]) 
}

y <- apply(dane, 1, sum)

igreki <- cbind(sukcesy=y,porazki=10-y)

model <- glm(igreki~pi, family="binomial")
summary(model)

1-pchisq(model$deviance, model$df.residual)
(1 - model$deviance / model$null.deviance)*100

r_d <- residuals(model, type='deviance')
r_p <- residuals(model , type='pearson')
r_resp <- residuals(model, type='response')

plot(r_d)
plot(r_d, predict(model, type='link'))
plot(r_d, predict(model, type='response'))
prplot(model,1)

# po dodaniu szumu jest duzo slabiej

######################## Zad.2.3 ###############################

conc <- 0:4
dead <- c(2,8,15,23,27)
number <- rep(30,5)

bliss <- data.frame(conc, dead, number)
bliss

alive <- number-dead
dane <- data.frame(dead, alive)
dane <- as.matrix(dane)
dane

model <- glm(dane~conc, family=binomial(link=logit), data=bliss)
summary(model)

# a)

# z summary -> conc jest istotny

1 - pchisq(model$null.deviance - model$deviance, 
           model$df.null - model$df.residual) # istotne

model0 <- glm(dane~1,data=bliss,family='binomial')
anova(model0, model, test='Chisq')            # to samo

# b)

1 - pchisq(model$deviance, model$df.residual)  # bardzo dobre dopasowanie

# c)

(1 - model$deviance / model$null.deviance)*100  # wysokie pseudo R^2 :D

# d)

model2 <- glm(dane~conc+I(conc^2), data=bliss, family="binomial")
summary(model2)
anova(model, model2)  # dodatkowa zmienna nie jest istotna

######################## Zad.2.4 ###############################

conc <- 0:4
dead <- c(2,8,15,23,27)
number <- rep(30,5)

conc2 <- rep(conc, each=30)
dead2 <- numeric(30*5)
for(i in 1:5){
   dead2[(30*(i-1)+1):(30*i)] <- c(rep(1,dead[i]),rep(0,30-dead[i]))
}

bliss2 <- data.frame(conc2, dead2)

modelindyw <- glm(dead2~conc2, data=bliss2, family="binomial")
summary(modelindyw)
summary(model)

# wspolczynniki takie same

model$deviance
modelindyw$deviance   # inne...

model0indyw <- glm(dead2~1, data=bliss2, family="binomial")
model0 <- glm(dane~1, data=bliss, family="binomial")

# wartosc statystyki opartej na dewiancjach:

anova(model, model0, test="Chisq")
anova(modelindyw, model0indyw, test="Chisq")   # to samo

######################## Zad.2.5 ###############################

malaria <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych\\malaria.data",
                       header=TRUE)
malaria

# a)

model <- glm(cbind(Spositive, Number-Spositive)~Age, data=malaria,
             family="binomial")
summary(model)

# b)

pstwo <- 0.25
x <- (log(pstwo/(1-pstwo)) - model$coefficients[1]) / model$coefficients[2] 
x   # ~36

predict(model, data.frame)

# c)

predict(model, data.frame(Age=20), se.fit=T, type='response')

# d)

pstwo <- numeric(100)

for(i in 1:100){
   pstwo[i] <- predict(model, data.frame(Age=i), type='response')
}

plot(1:100, pstwo, type="l")

# albo:

plot(malaria$Age, malaria$Spositive/malaria$Number)
lines(malaria$Age, model$fit, col='red',type='l')

######################## Zad.3.1 ###############################

finance <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych\\finance.data",
                      header=TRUE)
finance

# a)

model <- glm(Outcome~., data=finance, family="binomial")
summary(model)

1-pchisq(model$deviance, model$df.residual)  # sa zmienne istotne, dobrze dopasowany

(1 - model$deviance / model$null.deviance)*100  # 55%, srednio

# b)

modelstep <- step(model, direction="backward")
summary(modelstep)

anova(modelstep, model, test="Chisq")  # mniejszy model lepszy
(1 - modelstep$deviance / modelstep$null.deviance)*100  # 51% - faktycznie duzo nie stracilismy...

# c)

library("faraway")

# standaryzowane reszty dewiancyjne (dielone przez sqrt(1-h_ii))

rez <- rstandard(modelstep) 
qqnorm(rez)
qqline(rez, col='red')   # reszty w miare ok

halfnorm(rez) 
# reszty w miare ok, standaryzowane reszty w miare 
# z rozkladu normalnego

which(abs(rez)>2)
# obserwacje nr 16 i 34 sa wplywowe, wg odleglosci Cooka

which(abs(model$residuals)/sqrt(1-hatvalues(model))>2) 
# obserwacje potencjalnie odstajace

# d)

finance <- finance[-which(abs(model$residuals)/sqrt(1-hatvalues(model))>2)]

model <- glm(Outcome~., data=finance, family="binomial")
summary(model)

1-pchisq(model$deviance, model$df.residual)

(1 - model$deviance / model$null.deviance)*100

modelstep <- step(model, direction="backward")
summary(modelstep)

anova(modelstep, model, test="Chisq")  
(1 - modelstep$deviance / modelstep$null.deviance)*100  

# niewiele sie zmienilo...

######################## Zad.3.2 ###############################

heart <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych\\SAheart4.data",
                      header=TRUE)
head(heart)

# a)

family <- ifelse(heart$famhist=="Present", 1, 0)
heart <- cbind(heart, family)

head(heart)

# b)

model <- glm(chd~.-famhist-row.names-adiposity, data=heart, 
             family="binomial")
summary(model)

# c)

drop1(glm(chd~.-famhist-row.names-adiposity, data=heart, 
          family="binomial"), test="Chisq")
drop1(glm(chd~.-famhist-row.names-adiposity-sbp-obesity, data=heart, 
          family="binomial"), test="Chisq")
drop1(glm(chd~.-famhist-row.names-adiposity-sbp-obesity-alcohol, data=heart, 
          family="binomial"), test="Chisq")

modelstep <- step(model, direction="backward")
summary(modelstep)

######################## Zad.3.3 ###############################

library("rpart")

data(kyphosis)
kyphosis

# a)

g1 <- glm(Kyphosis~., data=kyphosis, family="binomial")
g2 <- glm(Kyphosis~.+I(Age^2)+I(Start^2)+I(Number^2), data=kyphosis, 
          family="binomial")
summary(g1)
summary(g2)

# b)

# tak, istotne są wszystkie (tak zupelnie koszernie, to tylko Start)

# c)

gstep <- step(g2, direction="backward")
summary(gstep)
summary(g1)   # tu AIC jest wieksze

# testu dewiancji nie wolno, bo dane nie sa zgrupowane i 
# w ogole te modele nie sa zagniezdzone...

# ale test chi juz wolno

anova(g1, gstep, test="Chisq")   # model wiekszy jest lepszy

######################## Zad.3.4 ###############################

data(discoveries)
discoveries

# czy srednia liczba odkryc w roku jest stala?

# a)

plot(discoveries)

# b)

model0 <- glm(x~1, data=discoveries, family="poisson")
summary(model0)

model <- glm(x~time(discoveries), data=discoveries, family="poisson")
summary(model)

anova(model0, model, test="Chisq")  # nie jest stale w czasie, bo lepszy wiekszy model

modelkw <- glm(x~time(discoveries)+I(time(discoveries)^2), 
             data=discoveries, family="poisson")
summary(model)
summary(modelkw)

anova(model, modelkw, test="Chisq")
# lepszy wiekszy model

# jakies wykresiki jeszcze:

plot(residuals(model)~model$fit) #overdispersion
abline(0,0,col='red')

plot(residuals(model,type='pearson') ~ model$fit) #overdispersion
abline(0,0,col='red')

qqnorm(residuals(model))
halfnorm(residuals(model))

######################## Zad.4.1 ###############################

conc <- 0:4
dead <- c(2,8,15,23,27)
number <- rep(30,5)

bliss <- data.frame(conc, dead, number)
bliss

model1 <- glm(cbind(dead, number-dead) ~ conc,
             family="binomial", data=bliss)
model2 <- glm(cbind(dead, number-dead) ~ conc,
              family="binomial"(link="probit"), data=bliss)
model3 <- glm(cbind(dead, number-dead) ~ conc,
              family="binomial"(link="cloglog"), data=bliss)

summary(model1)
summary(model2)
summary(model3)

x <- seq(-2,8,by=0.1)

a <- predict(model1, newdata=data.frame(conc=x), type='response')
b <- predict(model2, newdata=data.frame(conc=x), type='response')
c <- predict(model3, newdata=data.frame(conc=x), type='response')

a2 <- predict(model1, newdata=data.frame(conc=x))
b2 <- predict(model2, newdata=data.frame(conc=x))
c2 <- predict(model3, newdata=data.frame(conc=x))

matplot(x, cbind(a,b,c), type='l')
matplot(x, cbind(a2,b2,c2), type='l')

## pi_a/pi_b
## a-logit, b-probit

plot(x, a/b, type='l')
plot((1-a)/(1-b)~x,type='l') # roznice na ogonkach

## b-logit, a-probit
plot(b/a~x,type='l')
plot((1-b)/(1-a)~x,type='l')

## a-logit, c-cloglog
plot(a/c~x,type='l')
plot((1-a)/(1-c)~x,type='l')

## a-logit, a-cloglog
plot(c/a~x,type='l')
plot((1-c)/(1-a)~x,type='l')

######################## Zad.4.2 ###############################

gala <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych\\gala_data.txt",
                    header=TRUE)
head(gala)

# a)

model <- lm(Species~.-Endemics, data=gala)
summary(model)

plot(model$fitted.values, model$residuals)
abline(0,0, col="red")   # heteroskedastycznosc!

bptest(model)

# b)

library("MASS")

a <- boxcox(model, lambda=seq(0, 0.5, length.out=20), plotit=TRUE)
lambda <- a$x[which.max(a$y)]
lambda <- round(lambda,2)
lambda

# (x^lambda - 1)/lambda -> przeksztalcenie boxa-coxa

model2 <- lm(I((Species^lambda-1)/lambda) ~ .-Endemics, data=gala)
summary(model2)

plot(model2$fitted.values, model2$residuals)
abline(0,0, col="red") 

bptest(model2)

# c)

modelpois <- glm(Species~.-Endemics, data=gala, family="poisson")
summary(modelpois)

plot(residuals(modelpois,type='deviance') ~ predict(modelpois,type='link'))
abline(0,0,col='red') # spoko reszty
halfnorm(rstandard(modelpois))

# d)

(1 - modelpois$deviance / modelpois$null.deviance)*100  
# lepiej niz w lm()

# e)

plot(gala$Species)
lines(model$fitted.values, col="blue")
lines(modelpois$fitted.values, col="red")

# f)

summary(model)
summary(modelpois)

# istotne w lm: Elevation, Adjacent
# istotne w poiss: wszystkie !

######################## Zad.4.3 ###############################

lung <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok5\\GLM\\zbiory_danych\\lungcanc.dat",
                   header=TRUE)
head(lung)

lung1=lung
death1=lung$death
death1[lung$death==1 & lung$follow>1]=0
lung1$death=death1
attach(lung1)
den<-tapply(freq,list(age=lung1$age,smoker=lung1$smoker),FUN=sum)
num<-tapply(lung1$death,list(age=lung1$age,smoker=lung1$smoker),FUN=sum)
inc<-num/den
inc

# b)

age1<-tapply(lung1$age,list(age=lung1$age,smoker=lung1$smoker),FUN=mean)
#obejrzec zbior age1
smoke1<-tapply(lung1$smoker,list(age=lung1$age,smoker=lung1$smoker),FUN=mean)
plot(age1,inc,type="n",ylab="Incidence rate",xlab="Age")
points(age1[smoke1==0],inc[smoke1==0],pch=1)
points(age1[smoke1==1],inc[smoke1==1],pch=2)
legend("leftrigth",legend=c("non-smoker","smoker"),pch=c(1,2))

# c)

head(lung1)

lung.glm <- glm(death~smoker+age+offset(log(freq)),
                family=poisson,data=lung1)
summary(lung.glm)

# d)

exp(lung.glm$coeff[2])  # te szanse sa 8.6 raza wieksze!

#e
new<-data.frame(age=rep(35:80,2),smoker=rep(0:1,each=46),freq=rep(20,92))
pred<-predict(lung.glm,newdata=new,type="response")/20
lines(35:80,pred[1:46])
lines(35:80,pred[47:92])

#f
lung.glm2 <- glm(death~smoker+age+I(age^2)+offset(log(freq)),family=poisson,data=lung1)
summary(lung.glm2)
plot(age1,inc,type="n",ylab="Incidence rate",xlab="Age")
points(age1[smoke1==0],inc[smoke1==0],pch=1)
points(age1[smoke1==1],inc[smoke1==1],pch=2)
legend(40,.03,legend=c("non-smoker","smoker"),pch=c(1,2))
pred<-predict(lung.glm2,newdata=new,type="response")/20
lines(35:80,pred[1:46])
lines(35:80,pred[47:92])
# czynnik kwadratowy jest istotny
#g nie chce mi sie

######################## Zad.4.4 ###############################

beetle <- read.table("beetle.txt",header=TRUE)
head(beetle)

# a)

model <- glm(cbind(affected, exposed-affected)~log(conc),
             data=beetle, family="binomial")
summary(model)

# b)

plot(log(beetle$conc), beetle$affected/beetle$exposed)
lines(log(beetle$conc), model$fitted.values, col="red")
# calkiem dobre dopasowanie :D

# c)

plot(log(beetle$conc), 
     log((beetle$affected+0.5)/(beetle$exposed-beetle$affected+0.5)))
lines(predict(model,type='link')~log(beetle$conc),col='red')

prplot(model,1)

# d)

halfnorm(rstudent(model))  # 2 i 6

mod_bez <- glm(cbind(affected,exposed-affected)~log(conc),
               data=beetle,family='binomial',subset=-c(2,6))
summary(mod_bez)

1-pchisq(mod_bez$deviance, df=mod_bez$df.residual) # nie jest dopasowany
1-pchisq(sum(residuals(mod_bez,type='pearson')^2),df=8) # slabe dopasowanie

#e
phi <- sum(residuals(mod_bez,type='pearson')^2)/(nrow(beetle)-3)
summary(mod_bez)$coeff

#wariancje odpowiedzi przed i po:
pi <- mod_bez$fit
(var_odp <- pi*(1-pi)*beetle$exposed[-c(2,6)])
var_odp_phi <- var_odp*phi
plot(var_odp_phi,col='blue')
points(var_odp)
#test F
1-pf((mod_bez$null-mod_bez$dev)/(mod_bez$dev/(nrow(beetle)-3)),1,nrow(beetle)-3) # istotna zmienna 

######################## Zad.5.1 ###############################

solder <- read.table("solder.txt")
head(solder)

# a)

model <- glm(skips~., data=solder, family="poisson")
summary(model)

# test dopasowania:

# h0: dobrze dopasowany
# h1: zle dopasowany

1-pchisq(model$deviance, model$df.residual)   # czyli dopasowanie jest zle

# b)

model2 <- glm(skips~., data=solder, family=negative.binomial(1))
summary(model2)
1-pchisq(model2$deviance, model2$df.residual)   # dobrze dopasowany!

# c)

model3 <- glm.nb(skips~., data=solder)
summary(model3)

model4 <- glm(skips~., data=solder, family=negative.binomial(4))
summary(model4)
1-pchisq(model4$deviance, model4$df.residual) # zle dopasowany...

######################## Zad.5.2 ###############################

lips <- read.table("lips.dat", header=TRUE)
head(lips)

# a)

model <- glm(obs~affpct+offset(log(exp)), family="poisson", data=lips)
summary(model)

1-pchisq(model$deviance, model$df.residual) # zle dopasowanie, zmienna istotna
100*(1-model$deviance/model$null.deviance) # 37%

halfnorm(rstudent(model))

# odstajace sa 2 i 42:

lips2 <- lips[-c(2,42),]

model <- glm(obs~affpct+offset(log(exp)), family="poisson", data=lips2)
summary(model)

1-pchisq(model$deviance, model$df.residual) # zle dopasowanie, zmienna istotna
100*(1-model$deviance/model$null.deviance) # 39%

# nic nie zmienilo wyrzucenie ich...

# c)

nowy <- glm(obs~affpct+offset(log(exp)), family="poisson", data=lips)
phihat <- sum(residuals(nowy,type="pearson")^2)/nowy$df.residual
summary(nowy,dispersion=phihat)

######################## Zad.6.1 ###############################

# a)

curve(dgamma(x, shape=0.5, scale=1), from=0, to=7)
curve(dgamma(x, shape=1.5, scale=1), from=0, to=7, add=TRUE, col="blue")
curve(dgamma(x, shape=3, scale=1), from=0, to=7, add=TRUE, col="red")

# b)

clot <- read.table("clot.data", header=TRUE)
clot

clot1 <- data.frame(conc=rep(clot$plasma,2),
                    time=c(clot$lot1,clot$lot2),
                    lot=rep(c("1","2"),c(9,9)))
clot1

# c)

plot(clot1$conc[which(clot1$lot==1)], clot1$time[which(clot1$lot==1)], type="l")
lines(clot1$conc[which(clot1$lot==2)], clot1$time[which(clot1$lot==2)], col="red")

# d)

plot(log(clot1$conc[which(clot1$lot==1)]), 
     1/(clot1$time[which(clot1$lot==1)]), type="l")
lines(log(clot1$conc[which(clot1$lot==2)]), 
      1/(clot1$time[which(clot1$lot==2)]), col="red")


gam_inv <- glm(time~log(conc), data=clot1,family=Gamma(link='inverse'))
summary(gam_inv)

gam_log <- glm(time~log(conc)+lot, data=clot1,family=Gamma(link='log'))
summary(gam_log)

# e)

plot(residuals(gam_inv) ~ log(clot1$conc)) # widoczna zaleznosc...

#test dewiancji: nie umiem...

gam_inv_cos <- glm(time~log(conc)+lot,data=clot1,family=Gamma(link='inverse'))
res2 <- residuals(gam_inv_cos)
plot(res2~log(clot1$conc))

## sciagnij...

######################## Zad.7.1 ###############################

#7.1

#a)

gala <- read.table("gala_data.txt", header=T)
attach(gala)

g <- glm(Species~.-Endemics, data=gala, family=poisson)
beta<-g$coefficients
summary(g)

mi <- predict(g, gala ,type = "response") # do mi
eta <- predict(g, gala ,type = "link") # link do ety

d1 <- residuals(g)

plot(d1~mi)
plot(d1~eta) #powinien byc liniowy

#b)

r<-residuals(g, "response")

plot(r~mi)
plot(r~eta) #moze byc wzrastajaca war;

#c)

plot(Species~Area , data=gala) #trzeba przek zmienna area moze log?

plot(Species~log(Area) , data=gala)

#d)
z=eta+(Species-mi)/mi #linearized response
plot(z~log(Area) ) #widac liniowos ale uwzglenijmy wplyw innych zmiennych; powinno byc mi z modelu tylko  z ta jedna zmienna

#e
g1 <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz+0.1) + log(Adjacent), 
          data=gala, family=poisson )
summary(g1)
beta1<-g1$coefficients[2]
#jak patrzymy na dev to widzimy ze sie poprawilo residual dev porownujemy stopnie swobody te same

#f
mi1<-predict(g1, gala ,type = "response") #do mi
eta1<-predict(g1, gala ,type = "link") 
z1=eta1+(Species-mi1)/mi1

czesciowe<-z1-eta1+beta1*log(Area)
plot(czesciowe~log(Area)) #powinno byc liniowe 
abline(0, 0.348445  ) #nie ma powodow do niepokoju
#g
plot(z1~ eta1) #cos w rodzaju lijniowosci ok

#h
#wykres halfnormal rezyduow typu jacknife-roznica pomiedZy 
#obserwowana odpowiedzia dla itego przyp

#rstandard - studentyzowane
#rstudent- rezyduow typu jacknife

j<-rstudent(g1)
sum(residuals(g1)^2) #daje dew

halfnorm(j)
gala[abs(j)>7,] #znajdzmy te obs
#moze odstajace?

#i
library(faraway)
gali <- influence(g1) #zwraca diagnostyczne wart;
halfnorm(gali$hat)

gala[gali$hat>0.55,] #obs 25 wplywowa

#j
c<-cooks.distance(g1)
halfnorm(c)


#k
#Narysowac wykres zmiennosci wyestymowanego parametru Scruz.
beta5<-g1$coeff[5]
plot(gali$coefficients[,5]) #jaka jest wrazliwosc par scruz od kolejnych obs.
#os y wyspy 


g3 <- glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz+0.1) + log(Adjacent), data=gala[-25,], family=poisson )
summary(g3)


cbind(g1$coefficients,g3$coefficients) #log(Scruz ) zmienia sie znak , dziwne!!!!!!!!!!!!


modpla <- glm(Species ~ log(Area)+log(Adjacent), family=poisson, gala)

######################## Zad.8.1 ###############################

#8.2
#a
miner<-read.table("miner2.data",h=T)
m<-matrix(c(miner[1:8,1],miner[,3]),ncol=4,dimnames=list(NULL,c("year","normal","mild","severe")))
m
#b
m2<-m[,2:4]
mns<-apply(m2,1,sum)
mns
m3<-sapply(1:8,function(x){m2[x,]/mns[x]})
m3<-t(m3)
m3
m4<-cbind(m[,1],m3)
m4
#c
plot(m4[,1],m4[,2],type="l",ylim=c(0,1))
lines(m4[,1],m4[,3],col="red")
lines(m4[,1],m4[,4],col="blue")
#d
mpol<-cbind(m[,1:2],m[,3]+m[,4])
colnames(mpol)<-c("year","normal","nonnormal")
mpol<-as.data.frame(mpol)
mpol
model1<-glm(cbind(nonnormal,normal)~year,family=binomial,data=mpol)
summary(model1)
1-pchisq(model1$deviance,model1$df.residual)
library(faraway)
halfnorm(residuals(model1))
#e
model2<-glm(cbind(nonnormal,normal)~log(year),family=binomial,data=mpol)
summary(model2)
1-pchisq(model2$dev,6)
halfnorm(residuals(model2))

#8.3
data(nes96)
dane<-nes96
head(dane)
dane1<-dane[,6:9]
head(dane1)
dane1[,1]<-sapply(dane1[,1],function(x){
   if(any(x==c("strDem","weakDem"))){
      return("Democrats")
   }else if(any(x==c("indDem","indind","indRep"))){
      return("Independent")
   }else if(any(x==c("strRep","weakRep"))){
      return("Republican")
   }
})
head(dane1)
prop<-c(sum(dane1[,1]=="Republican"),sum(dane1[,1]=="Independent"),sum(dane1[,1]=="Democrats"))
prop
prop<-prop/sum(prop)
prop
#b
class(unclass(dane1$income))
inca <- c(1.5,4,6,8,9.5,10.5,11.5,12.5,13.5,14.5,16,18.5,21,
          23.5,27.5,32.5,37.5,42.5,47.5,55,67.5,82.5,97.5,115)
nincome<-inca[unclass(dane1$income)]
nincome
summary(nincome)
sd(nincome)
#c
head(dane1)
unique(dane1$educ)
dane11<-dane1[dane1$educ=="HS",]
dane12<-dane1[dane1$educ=="Coll",]
dane13<-dane1[dane1$educ=="BAdeg",]
dane14<-dane1[dane1$educ=="HSdrop",]
dane15<-dane1[dane1$educ=="CCdeg",]
dane16<-dane1[dane1$educ=="MS",]
dane17<-dane1[dane1$educ=="MAdeg",]
prop1<-c(sum(dane11[,1]=="Republican"),sum(dane11[,1]=="Independent"),sum(dane11[,1]=="Democrats"))
prop1<-prop1/sum(prop1)
prop2<-c(sum(dane12[,1]=="Republican"),sum(dane12[,1]=="Independent"),sum(dane12[,1]=="Democrats"))
prop2<-prop2/sum(prop2)
prop3<-c(sum(dane13[,1]=="Republican"),sum(dane13[,1]=="Independent"),sum(dane13[,1]=="Democrats"))
prop3<-prop3/sum(prop3)
prop4<-c(sum(dane14[,1]=="Republican"),sum(dane14[,1]=="Independent"),sum(dane14[,1]=="Democrats"))
prop4<-prop4/sum(prop4)
prop5<-c(sum(dane15[,1]=="Republican"),sum(dane15[,1]=="Independent"),sum(dane15[,1]=="Democrats"))
prop5<-prop5/sum(prop5)
prop6<-c(sum(dane16[,1]=="Republican"),sum(dane16[,1]=="Independent"),sum(dane16[,1]=="Democrats"))
prop6<-prop6/sum(prop6)
prop7<-c(sum(dane17[,1]=="Republican"),sum(dane17[,1]=="Independent"),sum(dane17[,1]=="Democrats"))
prop7<-prop7/sum(prop7)
mprop<-t(matrix(c(prop6,prop4,prop1,prop2,prop5,prop3,prop7),nrow=3,dimnames=list(c("Republican","Independent","Democrats"),c("HS","Coll","BAdeg","HSdrop","CCdeg","MS","MAdeg"))))
mprop
matplot(mprop,type="l")
#d
il <- c(8,26,42,58,74,90,107)
?cut
y<-cut(nincome,7)
?factor
z<-factor(x=y,labels=il)
#e
al <- c(24,34,44,54,65,75,85)
ag<-cut(dane1$age,7)
zage<-factor(x=ag,labels=al)
zage
#f
library(nnet)
dane1 <- cbind(dane1, nincome)
names(dane1)
library("nnet")
mmod1 <- multinom(PID~age+educ+nincome, data= dane1)
#g
mmodi <- step(mmod1, direction = "backward", k = log(nrow(dane1))) # BIC
mmodi
# zostaje nincome
#h
mmod2 <- multinom(PID~age+nincome, data= dane1)
summary(mmod2)
#1-pchisq(mmod2$deviance, nrow(dane1)) jak dopasowaine sprawdzic?
#i
predict(mmodi, newdata=data.frame(nincome=il))
#j to wynika chyba z teori i uwarunkowania modelu
#k to juz tez jakies rozwazania wyinkajace z teorii,
# to chyba mialo byc zadanie pouczajace ;p 

## ZAD 9.1
gat <- read.table('gator.data',header=TRUE)
#a

library('nnet')
head(gat)

attach(gat)
gat
gat_rozw <- data.frame(lake=factor(rep(lake,times=count)),
                       gender=factor(rep(gender,times=count)),
                       size=factor(rep(size,times=count)),
                       food=factor(rep(food,times=count)))
gat_rozw

mod_null <- multinom(food~1,data=gat_rozw)
summary(mod_null)
mod1_1 <- multinom(food~gender,data=gat_rozw)
summary(mod1_1)
mod1_2 <- multinom(food~size,data=gat_rozw)
summary(mod1_2)
mod1_3 <- multinom(food~lake,data=gat_rozw)
summary(mod1_3)
mod2_1 <- multinom(food~lake+gender,data=gat_rozw)
summary(mod2_1)
mod2_2 <- multinom(food~lake+size,data=gat_rozw)
summary(mod2_2)
mod2_3 <- multinom(food~gender+size,data=gat_rozw)
summary(mod2_3)
mod3 <- multinom(food~lake+gender+size,data=gat_rozw)
summary(mod3)
mod_full <- multinom(food~lake*gender*size,data=gat_rozw)
summary(mod_full)
#jak sie zmienia zmienna referencyjna - mocno recznie, w multinomie nie widze takiej opcji
# gat_rozw$food <- relevel(gat_rozw$food,ref='1')
#install.packages('VGAM')
library('VGAM')
?vglm
#taka ciekawostka, mozna innym pakietem dopasowac ten model
b <- vglm(food~lake+gender+size,multinomial(refLevel=1),gat_rozw)
summary(b)
str(mod_full)
mod_full$df
#b
#zasadnicze pytanie - dlaczego w modelu wysyconym wyswietlana dewiancja nie jest rowna 0 ?
# To jest dewiancja wzgledem czego ?
# i co mi daje takie obliczenie roznicy dewiancji, chyba powinno sie to przetestowac
modele <- list(mod1_1,mod1_2,mod1_3,mod2_1,mod2_2,mod2_3,mod3,mod_full)
lapply(modele, function(x){
   x$dev-mod_full$dev
})
###c
#zwykle pominiecie zmiennej plec
model1 <- multinom(food~size,data=gat_rozw)
summary(model1)
model2 <- multinom(food~lake,data=gat_rozw)
summary(model2)
model3 <- multinom(food~lake+size,data=gat_rozw)
summary(model3)
model_f <- multinom(food~lake*size,data=gat_rozw)
summary(model_f)
modele <- list(model1,model2,model3,model_f)
lapply(modele, function(x){
   x$dev-model_f$dev
})
###d
gat_rozw
library(dplyr)
obs <- gat %>%
   group_by(lake, size, food) %>%
   summarise(n=sum(count))
size_lake <- gat %>%
   group_by(lake, size) %>%
   summarise(n=sum(count))
# lake=1, size=1 (intercept)
obs$ocz[1:5] <- size_lake$n[1]*model3$fit[1,] # takie sa oczekiwane licznosci dla roznych kategorii food
# lake=1, size=2
obs$ocz[6:10] <- size_lake$n[2]*model3$fit[14,]
#jezioro1 i size2
obs$ocz[6:10] <- model3$fit[14,]*size_lake$n[2]
#jezioro2 i size1
obs$ocz[11:15] <- model3$fit[56,]*size_lake$n[3]
#jezioro2 i size2
obs$ocz[16:20] <- model3$fit[61,]*size_lake$n[4]
#jezioro3 i size1
obs$ocz[21:25] <- model3$fit[104,]*size_lake$n[5]
#jezioro3 i size2
obs$ocz[26:30] <- model3$fit[116,]*size_lake$n[6]
#jezioro4 i size1
obs$ocz[31:35] <- model3$fit[157,]*size_lake$n[7]
#jezioro4 i size2
obs$ocz[36:40] <- model3$fit[184,]*size_lake$n[8]
#e
# nie wiem co tu sie dzieje...
# statystyka testu Chi^2
1-pchisq(sum((obs$n-obs$ocz)^2/obs$ocz) ,df=12) #
#f
# trzeba zmienic kodowanie, do interceptu wrzucic size>2.3m oraz jezioro George (czyli ostatnie poziomy czynnikow)
gat_rozw$size <- relevel(gat_rozw$size,ref='2')
gat_rozw$lake <- relevel(gat_rozw$lake,ref='4')
model3 <- multinom(food~lake+size,data=gat_rozw)
summary(model3)
library(MASS)
summary(model3,cor=F) # po co ten cor, to ja nie wiem, pewnie nic nie robi...
#g
predict(model3,newdata=data.frame(size='2',lake='1'),type='probs')[2]
model3$fit[14,2] # ten croc bedzie zarl najprawdopodobniej rybki
#h
# w obu czynnikach ref - 1 poziom,
# trzeba na nowo stworzyc zbior gat_rozw
l1 <- multinom(food~lake+size,data=gat_rozw)
summary(l1)
# w obu czynnikach ref - ostatni poziom,
gat_rozw$size <- relevel(gat_rozw$size,ref='2')
gat_rozw$lake <- relevel(gat_rozw$lake,ref='4')
l2 <- multinom(food~lake+size,data=gat_rozw)
summary(l2)
# size - ostatni poziom, lake 3 poziome
gat_rozw$size <- relevel(gat_rozw$size,ref='2')
gat_rozw$lake <- relevel(gat_rozw$lake,ref='3')
l3 <- multinom(food~lake+size,data=gat_rozw)
summary(l3)
# oczywiscie zmiany we wspolczynnikach musza byc, ale juz w szacowanym pstwie nie powinny byc
predict(l1,newdata=data.frame(size='2',lake='3'),type='probs')
predict(l2,newdata=data.frame(size='2',lake='3'),type='probs')
predict(l3,newdata=data.frame(size='2',lake='3'),type='probs')
# rzeczywiscie tak jest, oszacowane pstwa troche sie roznia, ale to pewnie bledy numeryczne

### ZAD 10.1
imp <- read.table('impair.data',header=TRUE)
head(imp)

imp$mental <- factor(imp$mental,labels=c('dobry','lagodny','umiarkowany','zaburzony'),ordered=TRUE)
attach(imp)
library(MASS)
head(imp)
#a
mod1 <- polr(mental~ses+events,data=imp)
summary(mod1)

#b
predict(mod1,newdata=data.frame(ses=0,events=4.275),type='probs') # to sa poszczegolne pstwa, oczywiscnie nieskumulowane
p1 <- -0.2819-(-1.1112*0+0.3189*4.275)
f1 <- exp(p1)/(1+exp(p1)) # skumulowane P(Y<=dobry|x)
p2 <- 1.2128-(-1.1112*0+0.3189*4.275)
f2 <- exp(p2)/(1+exp(p2)) # P(Y<=lagodny|x)
p3 <- 2.2094-(-1.1112*0+0.3189*4.275)
f3 <- exp(p3)/(1+exp(p3)) # P(Y<=umiarkowany|x)
diff(c(0,f1,f2,f3,1)) # to sa pstwa Y= cos, czyli zgadza sie
#c
range(imp$events)
probs <- apply(mod1$fit[,c(3,4)],1,sum) # pstwa Y - umiarkowany lub zaburzony
events
kol=rep(1,40)
kol[which(ses==0)] <- 2
plot(probs~events,col=kol) # na czerwono biedniejsi. Widac, że w obu grupach wraz ze wzrostem eventsow,
# pstwo rosnie i to proporcjonalnie,
#d
summary(mod1)
exp(-mod1$coeff[1]) # YEAH, That's right ! OR =3
#e
(a <- predict(mod1,newdata=data.frame(ses=0,events=4.275),type='probs')[1])
(b <- predict(mod1,newdata=data.frame(ses=1,events=4.275),type='probs')[1])
# te obliczenia wiele nie mowia, oprocz tego, ze ses ma duzy wplyw...
#f
#ses=0
stat <- summary(events)
(a <- predict(mod1,newdata=data.frame(ses=0,events=stat[2]),type='probs')[1])
(b <- predict(mod1,newdata=data.frame(ses=0,events=stat[5]),type='probs')[1])
# dla biednych roznica jest spora
(a <- predict(mod1,newdata=data.frame(ses=0,events=stat[1]),type='probs')[1])
(b <- predict(mod1,newdata=data.frame(ses=0,events=stat[6]),type='probs')[1])
# so ?

### ZAD 2
gat <- read.table('gator.data',header=TRUE)
gat$lake
#a
tab <- aggregate(count~lake+food,FUN=sum,data=gat)
tab$lake <- factor(tab$lake)
tab$food <- factor(tab$food)
#tak sobie robie - test Chi^2 Pearsona i LRT test
?xtabs
w <- xtabs(count~lake+food,data=tab)
summary(w) # wiadomo, ze aproksymacja chi^2 moze byc niewlasciwa
#install.packages('gmodels')
library(gmodels)
library(epitools)
# LRT test
1-pchisq(2*sum(w*log(w/expected(w))),12)
#b

mod1 <- glm(count~lake+food,family=poisson,data=tab)
mod1_int <- glm(count~lake*food,family=poisson,data=tab)
summary(mod1)
summary(mod1_int)
1-pchisq(mod1$deviance,df=mod1$df.residual)
# nie ma niezaleznosci

#c
aggregate(count~lake,FUN=function(count){sum(count)/sum(w)},data=tab) # frakcje z danych
apply(matrix(mod1$fitted.values,ncol=5),1,function(x) sum(x)/sum(w)) # to samo, interesujace...
# to samo, bo takie sa wlasnie esymatory MLE - frakcje z proby

## ZAD 3
library(faraway)
data(femsmoke)
?femsmoke
head(femsmoke)
#b

tab <- xtabs(y~smoker+dead, data=femsmoke)
tab
props <- prop.table(tab,1)
props

tab <- xtabs(y~smoker+dead,data=femsmoke)
tab
props <- prop.table(tab,1)
or=prod(diag(props))/(prod(props[c(2,3)])) # Odds Ratio 0.6848366 - 'szansa' na smierc po 20latach
# jest o ok. 30% nizsza u palacych niz niepalacych - People are Strange - The Doors
#c i d jednoczesnie
w <- xtabs(y~smoker+dead+age,data=femsmoke)
for (i in 1:length(levels(femsmoke$age))){
   print(levels(femsmoke$age)[i])
   print(prop.table(w[,,i],1))
}
#d
ory <- numeric(length(levels(femsmoke$age)))
for (i in 1:length(levels(femsmoke$age)) ){
   ory[i] <- prod(diag(w[,,i]))/(w[1,2,i]*w[2,1,i])
}
# ORy - widac, ze w prawie kazdej grupie wiekowej maja wieksza szanse zgonu, oprocz
# w kategorii 0.75
matrix(round(ory,3),ncol=7,dimnames=list(c(''),levels(femsmoke$age)))

#f
mod1 <- glm(y~smoker+dead+age,data=femsmoke,family=poisson) #
# interpretacja wspolczynnika smoke: grupa osob niepalacych, w wieku 18-24 ktorzy umarli jest wieksza niz palacych i umarli w wieku 18-24
summary(mod1)
1-pchisq(mod1$dev,df=mod1$df.res) # nie jest dobrze dopasowany, sa interakcje, nie ma niezaleznosci tych trzech cech

#g
mod2 <- glm(y~smoker*dead+age,data=femsmoke,family=poisson)
1-pchisq(mod2$dev,df=mod2$df.res)
# nie jest dobrze dopasowany, wiek jest zalezny z para (dead,age)
#h
mod3 <- glm(y ~ smoker*age + age*dead, femsmoke, family=poisson)
summary(mod3)
# odpowiada to niezaleznosci: smoker i dead niezalezne warunkujac na wiek
1-pchisq(mod3$dev,df=mod3$df.res) # owszem, jest taka niezaleznosc
#i
mod4 <- glm(y ~ (smoker+age+dead)^2, femsmoke, family=poisson)
summary(mod4)
# jest to model ze wszystkimi interakcjami drugiego rzedu
1-pchisq(mod4$dev,df=mod4$df.res)
or <- numeric(7)
fity <- lapply(split(mod4$fit,femsmoke$age),function(x) matrix(x,ncol=2))
for (i in 1:7){
   a <- fity[[i]]
   or[i] <- prod(diag(a))/(a[2]*a[3])
}
# rzeczywiscie w tym modelu wszystkie ORy sa takie same
#j
mod5 <- glm(y ~ smoker*age*dead, data=femsmoke, family=poisson)
anova(mod4,mod5, test="Chisq") # mozna wywalic interakcje

#11.1
uzywki<-read.table("uzywki.txt",h=T)
head(uzywki)
#a
model1<-glm(y~alcohol+cigarettes+marijuana,family=poisson,data=uzywki)
model2<-glm(y~alcohol*cigarettes+marijuana,family=poisson,data=uzywki)
model3<-glm(y~alcohol*marijuana+cigarettes*marijuana,family=poisson,data=uzywki)
model4<-glm(y~alcohol*marijuana+cigarettes*marijuana+alcohol*cigarettes,family=poisson,data=uzywki)
fitted(model1)
fitted(model2)
fitted(model3)
fitted(model4)
summary(model4)
#b
theta_AC1_1<-539.98258*47.32880/(282.09123*90.59739)
theta_AC0_1<-740.22612*64.87990/(386.70007*124.19392)
theta_AC_1<-(539.98258 +740.22612)*(47.32880+64.87990 )/((282.09123+386.70007)*(90.59739+124.19392))
theta_AM1_3<-909.2395833*142.1595745/(438.8404255*4.7604167)
theta_AM0_3<-45.7604167*179.8404255/(555.1595745 *0.2395833)
theta_AM_3<-(909.2395833+45.7604167)*(142.1595745+179.8404255)/((438.8404255+555.1595745)*(4.7604167+0.2395833))
#d
model4$dev
names(model4)
model4$df.res
sum(residuals(model4,"pearson")^2)
#f
#by sprawdziæ war. niezal., porównujemy model 3 z 4
summary(model3)
summary(model4)
1-pchisq(model3$dev-model4$dev,1)
#czyli s¹ warunkowo zale¿ne
#zad.2
data(HairEyeColor)
dane<-HairEyeColor
dane1<-dane[,,1]+dane[,,2]
dane2<-matrix(0,16,3)
dane
for(i in 1:4){
   for(j in 1:4){
      dane2[i+(j-1)*4,1]<-i
      dane2[i+(j-1)*4,2]<-j
      dane2[i+(j-1)*4,3]<-dane1[i,j]
   }
}
colnames(dane2)<-c("Hair","Eye","y")
dane2[,1]<-factor(dane2[,1],labels=c("Black","Brown","Red","Blond"))
dane2[,2]<-factor(dane2[,2],labels=c("Brown","Blue","Hazel","Green"))
dane2
dane3<-xtabs(y~Hair+Eye,data=dane2)
dane3
colnames(dane3)<-c("Black","Brown","Red","Blond")
rownames(dane3)<-c("Brown","Blue","Hazel","Green")
summary(dane3)
dotchart(dane3)
mosaicplot(dane3)

#12.1
#a)
library(PBImisc)
library(dplyr)
data(milk)
head(milk)
srednie <- milk %>% group_by(cow) %>% 
   summarise(srednia = mean(milk.amount))

#b)
library(lattice)
dotplot(cow~milk.amount,data=milk,xlab="mlecznosc [kg/dzien]")

#c)

library(lme4)

model1<-lmer(milk.amount~(1|cow), data=milk)
model2 <- lmer(milk.amount~1+(1|cow),data=milk)
summary(model1)
summary(model2)

# Linear mixed model fit by REML ['lmerMod']
# Formula: milk.amount ~ 1 + (1 | cow)
# Data: milk
# 
# REML criterion at convergence: 178.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.9981 -0.4136  0.1775  0.6561  1.4021 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# cow      (Intercept) 7.589    2.755   
# Residual             3.000    1.732   
# Number of obs: 40, groups:  cow, 10
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  27.0150     0.9132   29.58


#d)

model3 <- glm(milk.amount~cow, data=milk)
zwykly <- model3$coefficients
zwykly[2:10] <- zwykly[2:10]+ zwykly[1]

mieszany <- ranef(model2)$cow + rep(fixef(model2),10)

#ranef() - efekt losowy, oszacowanie dla każdej krowy
#fixef() - efekt stały - srenia globalna

predykcja<- cbind(srednie$srednia, as.numeric(zwykly), mieszany)
colnames(predykcja) <- c("srednie", "liniowy", "mieszany")
predykcja



#12.2

bales <- read.table("bales.txt")
head(bales)

bales2 <- as.factor(rep(names(bales), each=4))
bales3 <- c(bales[,1], bales[,2], bales[,3], bales[,4], bales[,5], bales[,6], bales[,7])
bales3

bal <- data.frame(bales=bales2, count=bales3)
bal

boxplot(bales)
dotplot(bales~count,data=bal)

stripchart(count~bales,data=bal)

#b) zwykly model liniowy 


modelz <- lm(count~bales, data=bal)
summary(modelz)
#p-value: 0.1573 wiec wszystkie alfy zerowe czyli srednie takie same

#c)
model<-lmer(count~(1|bales), data=bal)
summary(model)




























