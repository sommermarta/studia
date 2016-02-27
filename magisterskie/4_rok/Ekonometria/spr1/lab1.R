# 1.1

a <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Ekonometria\\lab1_zad1.csv",
                sep=";", header=TRUE)

y <- a$pop/(a$CPI*a$ludnosc)
x <- matrix(c(a$cena/a$CPI,a$doch/a$CPI),ncol=2)
colnames(x) <- c("cena2","dochod2")
y; x

liniowa <- lm(y~x)
summary(liniowa)

potegowa <- lm(log(y)~log(x))
summary(potegowa)

elast_cen_liniowy <- (liniowa$coefficients[2]*x[,"cena2"])/y
elast_cen_liniowy
elast_dochod_liniowy <- (liniowa$coefficients[3]*x[,"dochod2"])/y
elast_dochod_liniowy

elast_cen_potegowy <- exp(potegowa$coefficients[2])
elast_cen_potegowy
elast_dochod_potegowy <- exp(potegowa$coefficients[3])
elast_dochod_potegowy





























































































