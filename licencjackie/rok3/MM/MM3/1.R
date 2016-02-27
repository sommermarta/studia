# h.szymanowski@phd.ipipan.waw.pl
# pd1 MM

#zad1
D=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/MM/DANE/SBP.txt", header=T)
head(D)

plot(D$SBP ~ D$Age)
boxplot(D$SBP)
model1=lm(D$SBP~D$Age)    #dopasowuje model najmniejszych kwadratów
summary(model1)

str(summary(model1))
str(model1)

model1$residuals     #ró¿nica miêdzy estymacj¹, a prost¹
residuals(model1)    #to samo
y_hat = model1$fitted.values   #predykcje
D$SBP-y_hat                    # to samo, co residua
summary(model1)$r.squared      #zwraca R

plot(D$SBP~D$Age)
abline(model1)                #rysuje dopasowan¹ prost¹
abline(model1$coef[1], model1$coef[2], col="blue") #to samo, co wy¿ej

model1$coef              #wspó³czynniki prostej

which.max(D$SBP)         #wspó³rzêdna najwiêkszego elementu
wsp = which.max(D$SBP)
D1 = D[-wsp,]
D1
model2=lm(D1$SBP~D1$Age)
abline(model2, col="red")
summary(model1)$r.squared; summary(model2)$r.squared

plot(model1)             #wykres diagnostyczny ????????


#zad3
D=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/MM/DANE/geese.txt", header=T, sep=",")
head(D)
plot(D$Bestimate ~ D$photo)
plot(D$photo, D$Bestimate)    #to samo, co wy¿ej

model1 = lm(D$Bestimate ~ D$photo)
summary(model1)
#model1=lm(D$Bestimate~D$photo-1)         #¿eby byl wykres bez wyrazu wolnego

model2=lm(log(D$Bestimate)~D$photo)
plot(log(D$Bestimate)~D$photo)

model3=lm(D$Bestimate~I(log(D$photo)))
plot(D$Bestimate~I(log(D$photo)))     #dziêki temu wie, ¿e chodzi o logarytm, a nie o zmienn¹ logcoœtam

model3=lm(D$Bestimate~log(D$photo))
plot(D$Bestimate~log(D$photo))        #to wy¿ej, to jakaœ œciema ;p

model4=lm(log(D$Bestimate)~log(D$photo))
plot(log(D$Bestimate)~log(D$photo))          #wysz³o ³adne liniowe ;)

summary(model4)
abline(model4)


#zad4
data(pressure)
P=pressure
plot(P$pressure~P$temperature)
modelP=lm(P$pressure~P$temperature)
abline(modelP)

library(MASS)
boxcox(modelP, lambda=seq(0.1,0.15,length.out=100))

#bierzemy 0.12
p=P$pressure
modelB=lm((p^0.12-1)/0.12~P$temperature)
plot((p^0.12-1)/0.12~P$temperature)
summary(modelB)

v=modelB$coef

f=function(x){
  (0.12*(v[2]*x+v[1])+1)^(1/0.12)
}
f(5)
curve(f,from=0,to=400,add=T, col="red")

#zad2

D=read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/MM/DANE/windspeed.txt", header=T, sep=",")
head(D)

model1=lm(D$output~D$speed)
plot(D$output~D$speed)

model2=lm(log(D$output)~D$speed)
plot(log(D$output)~D$speed)

model3=lm(log(D$output)~log(D$speed))
plot(log(D$output)~log(D$speed))

model4=lm(D$output~log(D$speed))
plot(D$output~log(D$speed))

model5=lm(sqrt(D$output)~D$speed)
plot(sqrt(D$output)~D$speed)

model6=lm(sqrt(D$output)~sqrt(D$speed))
plot(sqrt(D$output)~sqrt(D$speed))

model7=lm(D$output~sqrt(D$speed))
plot(D$output~sqrt(D$speed))

#... itd
#najlepszy by³ chyba model nr 4, wiêc na nim bêdê pracowaæ:

model4=lm(D$output~log(D$speed))
plot(D$output~log(D$speed))
abline(model4, col='red')
summary(model1)$r.squared


















































