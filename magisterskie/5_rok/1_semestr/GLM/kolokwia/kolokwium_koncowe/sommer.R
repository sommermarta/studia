#####################################################
# Imie i nazwisko:  Marta Sommer
#####################################################
#
# Instalacja bibliotek:
install.packages(c("MASS","faraway"),repos=c("http://r.meteo.uni.wroc.pl/"))
library("MASS")
library("faraway")
library("rpart")
library("car")
library("lattice")
library("lme4")
library("dplyr")
library("acepack")
library("mgcv")
library("lmtest")
library("forward")
library("nnet")
library("mgcv")
#
#
#####################################################

#####################################################
# zadanie 1
#####################################################
# (a)

# przygotowanie danych:

osobowosc <- rep(c("A", "A", "B", "B"), 2)
cholesterol <- rep(c("normalny", "wysoki"),4)
cisnienie <- rep(c("normalne","wysokie"), each=4)
c.normalne <- c(716,207,819,186)
c.wysokie <- c(79,25,67,22)
licznosc <- c(c.normalne, c.wysokie)

dane <- data.frame(osobowosc, cholesterol, cisnienie, licznosc)
dane

pstwo <- sum(dane[which(cholesterol=="normalny"),]$licznosc)/sum(dane$licznosc)
pstwo/(1-pstwo)

# (1a) odpowiedz:

# Stosunek szans posiadania normalnego poziomu cholesterolu 
# wynosi 3.820455

#####################################################
# (b)

dane

model <- glm(licznosc~cholesterol+osobowosc+cisnienie, 
             data=dane, family="poisson")
summary(model)

predict(model, data.frame(osobowosc="A", cholesterol="normalny",
                          cisnienie="normalne"), type="response")
# (1b) odpowiedz:

# wynosi # 739.8844 

#####################################################
# (c)

model <- glm(licznosc~cholesterol+osobowosc+cisnienie, 
             data=dane, family="poisson")
model2 <- glm(licznosc~cholesterol*osobowosc*cisnienie, 
              data=dane, family="poisson")
summary(model2) # tu wyglada, jakby byly nieistotne

anova(model, model2, test="Chisq")
# p-value = 0.06841 > 0.05, czyli nie mamy podstaw do odrzucenia hipotezy,
# czyli mniejszy model jest lepszy, czyli faktycznie nie sa zalezne

# (1c) odpowiedz:

# Wszystkie zmienne sa niezalezne.

#####################################################
# (d)

model3 <- glm(licznosc~cholesterol*cisnienie + osobowosc, 
              data=dane, family="poisson")
summary(model3)

anova(model, model3, test="Chisq")
# p-value = 0.2029 > 0.05, czyli mniejszy model lepszy, czyli sa 
# sa niezalezne.

# (1d) odpowiedz:

# S¹ warunkowo niezalezne.

#####################################################
# (e)

summary(model2)
summary(model3)

anova(model3, model2, test="Chisq")

# p-value = 0.06873 > 0.05, czyli mniejszy model lepszy

# (1e) odpowiedz:

# Lepszy jest model z punktu d).

#####################################################

#####################################################
# zadanie 2
#####################################################
# (a)

czesci <- read.table("h:\\Windows7\\Desktop\\e\\czesci.txt",
                     header=TRUE)
head(czesci)

# dopasowanie modeli:

modell <- glm(cbind(damage, 6-damage)~temp, data=czesci, 
              family=binomial(link="logit"))
summary(modell)

modelp <- glm(cbind(damage, 6-damage)~temp, data=czesci, 
              family=binomial(link="probit"))
summary(modelp)

# sprawdzenie ich dopasowania:

1-pchisq(modell$deviance, modell$df.residual)
# 0.7164099 -> czyli model logitowy jest dobrze dopasowany

1-pchisq(modelp$deviance, modelp$df.residual)
# 0.640727 -> czyli model logitowy jest dobrze dopasowany

# wykresy:

plot(czesci$temp, modell$fitted.values, type="l")
lines(czesci$temp, modelp$fitted.values, type="l", col="blue")

# (2a) odpowiedz:

# Oba modele sa dobrze dopasowane.

#####################################################
# (b)

predict(modell, data.frame(temp=31), type="response")
# 0.9930342 -> bardzo duze pstwo

predict(modelp, data.frame(temp=31), type="response")
# 0.9895983  -> podobnie, bardzo duze pstwo

# (2b) odpowiedz:

# predykcja dla logitu: 0.9930342
# predykcja dla probitu: 0.9895983 

#####################################################
# (c)

summary(modell)

# (2c) odpowiedz:

# Z testu Walda wynika, ze jest ona istotna (p-value male = 4.78e-05)

#####################################################
# (d)

wsp <- summary(modell)$coef[2,1]  # wspolczynnik przy temp
se <- summary(modell)$coef[2,2]  # se wspolczynnika przy temp

wsp + c(-1,1)*se*qnorm(0.975) # przedzial ufnosci

# (2d) odpowiedz:

# Przedzial ufnosci dla tego wspolczynnika to: (-0.3204587; -0.1120086)

#####################################################
# (e)

exp(wsp)

# (2e) odpowiedz:

# Przy wzroscie temperatury o 1 stopien, szanse maleja okolo 0.8 raza. 
# Czyli o 20%.

#####################################################
# (f)

exp(wsp + c(-1,1)*se*qnorm(0.975))

# (2f) odpowiedz:

# Ten przedzial ufnosci wynosi wtedy: (0.7258160; 0.8940366)

#####################################################

#####################################################
# zadanie 3
#####################################################
# (a)

carpref <- read.table("h:\\Windows7\\Desktop\\e\\carpref.txt",
                     header=TRUE)
head(carpref)

mac <- matrix(carpref$frequency, ncol=3, byrow=TRUE)
wiek <- rep(unique(carpref$age),2)
plec <- rep(c(0,1),each=3)

nowedane <- data.frame(plec, wiek, mac)

model <- multinom(cbind(X1, X2, X3)~plec+wiek, data=nowedane)
summary(model)

modelnull <- multinom(cbind(X1, X2, X3)~1, data=nowedane)
summary(modelnull)

anova(modelnull, model, test="Chisq")
# odrzucamy hipoteze, czyli model wiekszy lepszy, czyli model 
# dobrze dopasowany

# (2a) odpowiedz:

# Dobra jakosc dopasowania.

#####################################################
# (b)

p <- predict(model, data.frame(plec=0, wiek="18-23"), type="probs")
p
p[1]

# (2b) odpowiedz:

# Pstwo tego, ze taka kobieta uzna dostepnosc klimatyzacji i wspomagania
# za niewazny/malo wazny czynnik wyniesie 0.5242

#####################################################

