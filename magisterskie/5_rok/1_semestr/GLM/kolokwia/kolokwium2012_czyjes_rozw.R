

#zadanie1
#wczytanie danych
dane=read.table("H:/GLM/GLM/plik.txt",h=T)
head(dane)
attach(dane)

#a
library(mgcv)
#jest to model addytywny
mod1=gam(y~s(x1)+s(x2),data=dane)

summary(mod1) # istotne jest s(x1), bo pval<0.05
#test dopasowania modelu
1-pchisq(mod1$deviance,mod1$df.residual) #0- model nie jest dobrze dopasowany

#b
#definicja funkcji:
f1=function(x,c) ifelse(x>c,x-c,0) 
f2=function(x,c) ifelse(x<c,c-x,0) 

plot(mod1)
mod2=lm(y~f1(x1,-2)+f2(x1,-2.5)+f1(x2,0)+f2(x2,0),data=dane)

summary(mod2) # istotne x1, bo pval<0.05
#test dopasowania modelu
summary(mod2)$r.squared  #0.9197426- model dosc dobrze dopasowany

#c
l=lm(y~x1+x2,data=dane)
#porownanie z mod1
anova(l,mod1,test="Chi") #pval 2.2e-16 -mod1 lepszy

#porownanie z mod1
anova(l,mod2,test="Chi") #pval 2.2e-16 -mod2 lepszy

#wylaczenie danych
detach(dane)

#zadanie2

#wczytanie danych
dead=c(15,17,22,38,144)
razem=c(297,242,312,299,285)
dawka=c(0.0,62.5,125.0,250.0,500.0)
szkod=data.frame(dead,razem,dawka)
head(szkod)
attach(szkod)

#a
#potrzebny jest model logistyczny

y=cbind(dead,razem-dead) # przygotowanie danych: sukces-smierc, porazka
mod=glm(y~dawka,family=binomial,data=szkod)

#b
summary(mod) #pval przy dawka<2e-16 , czyli zmienna dawka istotna

#c
#procent dewniancji wyjasnianej przez model
1-(mod$deviance/mod$null.deviance) # 0.9777024

#model wyjasnia 98% dewiancji

#wylaczenie danych
detach(szkod)

#zadanie 3
#wczytanie danych

rok   kwartal1 kwartal2 kwartal3 kwartal4
1987  217      144      177      131
1988  156      163      205      130
1989  149      137      189      117

rok=c(1987,1988,1989)
kwartal1=c(217,156,149)
kwartal2=c(144,163,137)
kwartal3=c(177,205,189)
kwartal4=c(131,130,117)
wypadki=data.frame(rok,kwartal1,kwartal2,kwartal3,kwartal4)
head(wypadki)
attach(wypadki)

#a
#trzeba zagregowac dane do tablicy kontyngencji
rok=c(rep(1987,4),rep(1988,4),rep(1989,4))
osoby=c(217,144,177,131,156,163,205,130,149,137,189,117)
kwartal=rep(c(1,2,3,4),3)
nowe=data.frame(rok,kwartal,osoby)
head(nowe)

#dopasowanie modelu: liczba poszkodowanych od kwartaly+rok
g=glm(osoby~rok+kwartal,family=poisson,data=nowe)
summary(g) #wszystkie zmienne istotne

#test dopasowania modelu
1-pchisq(g$deviance,g$df.residual) #3.426617e-08- model nie jest dobrze dopasowany

#wszystkie zmienne istotne, ale model zle dopasowany, czyli moze byc nadwyzka rozproszenia

#b
#oszacowanie parametru rozproszenia
phihat=sum(residuals(g,type="pearson")^2)/g$df.res #6.090873-parametr rozproszenia

#dopasowanie modelu z wyestymowanym parametrem rozproszenia
summary(g,dispersion=phihat)

#wylaczenie danych
detach(wypadki)


