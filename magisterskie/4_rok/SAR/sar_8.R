# 8.1

library("lars")
data(diabetes)
attach(diabetes)
head(diabetes)
ram <- cbind(diabetes$x,y=diabetes$y)
head(ram)

# a) dopasowac model uzywajac metody lasso

las <- lars(ram[,-11],ram[,11],type="lasso",trace=TRUE)

# LARS Step 1 :    Variable 3 	added
# LARS Step 2 :	 Variable 9 	added
# LARS Step 3 :	 Variable 4 	added
# LARS Step 4 :	 Variable 7 	added
# LARS Step 5 :	 Variable 2 	added
# LARS Step 6 :	 Variable 10 	added
# LARS Step 7 :	 Variable 5 	added
# LARS Step 8 :	 Variable 8 	added
# LARS Step 9 :	 Variable 6 	added
# LARS Step 10 :	 Variable 1 	added
# Lasso Step 11 :	 Variable 7 	dropped
# LARS Step 12 :	 Variable 7 	added

plot(las,breaks=FALSE)

# b) selekcja zmiennych

las$lambda
las$beta

#         age        sex       bmi       map        tc       ldl       hdl      tch      ltg      glu
# 0    0.0000    0.00000   0.00000   0.00000    0.0000   0.00000    0.0000   0.0000   0.0000  0.00000
# 1    0.0000    0.00000  60.11927   0.00000    0.0000   0.00000    0.0000   0.0000   0.0000  0.00000
# 2    0.0000    0.00000 361.89461   0.00000    0.0000   0.00000    0.0000   0.0000 301.7753  0.00000
# 3    0.0000    0.00000 434.75796  79.23645    0.0000   0.00000    0.0000   0.0000 374.9158  0.00000
# 4    0.0000    0.00000 505.65956 191.26988    0.0000   0.00000 -114.1010   0.0000 439.6649  0.00000
# 5    0.0000  -74.91651 511.34807 234.15462    0.0000   0.00000 -169.7114   0.0000 450.6674  0.00000
# 6    0.0000 -111.97855 512.04409 252.52702    0.0000   0.00000 -196.0454   0.0000 452.3927 12.07815
# 7    0.0000 -197.75650 522.26485 297.15974 -103.9462   0.00000 -223.9260   0.0000 514.7495 54.76768
# 8    0.0000 -226.13366 526.88547 314.38927 -195.1058   0.00000 -152.4773 106.3428 529.9160 64.48742
# 9    0.0000 -227.17580 526.39059 314.95047 -237.3410  33.62827 -134.5994 111.3841 545.4826 64.60667
# 10 -10.0122 -239.81909 519.83979 324.39043 -792.1842 476.74584  101.0446 177.0642 751.2793 67.62539

w <- which(min(las$Cp)==las$Cp)   # 7 (8) 
# Czyli nieistone sa: age, ldl, tch 

# c) wartosci estymatorow w wybranym modelu

las$beta[8,]

#        age        sex        bmi        map         tc        ldl      
#    0.00000 -197.75650  522.26485  297.15974 -103.94625    0.00000 
#        hdl        tch        ltg        glu 
# -223.92603    0.00000  514.74948   54.76768 

# d) pacjent opisany zmiennymi bedacymi medianami atrybutow z macierzy x - 
#    dokonac predykcji dla tego pacjenta

pac <- numeric(ncol(ram)-1)
for(i in 1:(ncol(ram)-1)){
   pac[i] <- median(ram[,i])
}
pac
pac <- matrix(pac,nrow=1)

predict.lars(las,newx=as.matrix(pac),s=8,mode="step")   # 152.3675


# 8.2

m <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                Miasta.txt",header=TRUE)
head(m)

#             Work Price Salary
# Amsterdam   1714  65.6   49.0
# Athens      1792  53.8   30.4
# Bogota      2152  37.9   11.5
# Bombay      2052  30.3    5.3
# Brussels    1708  73.8   50.5
# BuenosAires 1971  56.1   12.5

# a) standaryzacja zmiennych

mm <- scale(m)

# inny sposob (taki po kolei)

sr <- numeric(3)
sd <- numeric(3)

for(i in 1:3){
   sr[i] <- mean(m[,i])
   sd[i] <- sd(m[,i])
}

for(i in 1:3){
   m[,i] <- (m[,i]-sr[i])/sd[i]
}

# b) dla Work i Price wyznaczyc kierunki wzdluz ktorych wystepuje najwieksza
#    zmiennosc; wykres rozproszenia

pc1 <- princomp(~., cor=FALSE, data=as.data.frame(mm[,1:2])) 

# wybieramy albo macierz korelacji albo kowariancji - mamy 
# zestandaryzowane dane, wiec juz korelacja nie jest potrzebna

names(pc1) 
# "sdev"     "loadings" "center"   "scale"    "n.obs"    "scores"   "call" 

pc1$scores    # Comp.1 to z1, a Comp.2 to z2

#                   Comp.1      Comp.2
# Amsterdam     0.52415206  0.82168390
# Athens       -0.18230132  0.89542510
# Bogota       -2.16804595 -0.03904038
# Bombay       -2.01371032  0.61779346
# Brussels      0.81957171  0.57493442
# BuenosAires  -0.83226209  0.09339250

pc1$loadings  # wektory ladunkow (pierwsza kolumna to a1, a druga to a2)

# Loadings:
# Comp.1 Comp.2
# Work  -0.707 -0.707
# Price  0.707 -0.707

plot(mm[,1],mm[,2])
abline(c(0,-1),col="red")
abline(c(0,1),col="blue")

# c) analiza skladowych glownych dla wszystkich zmiennych

pc2 <- princomp(~., cor=FALSE, data=as.data.frame(mm))
pc2$loadings          # puste miejsce oznacza 0

# d) Jaki procent wariancji tlumaczony jest przez poszczegolne skladowe? 
#    Czy mozemy dokonac redukcji wymiaru danych?

summary(pc2) # tu moznaby wziac dwie pierwsze skladowe glowne

# Importance of components:
#                           Comp.1    Comp.2    Comp.3
# Standard deviation     1.4536997 0.7935060 0.4380504
# Proportion of Variance 0.7200679 0.2145480 0.0653841
# Cumulative Proportion  0.7200679 0.9346159 1.0000000

plot(pc2)    # wariancje, czyli w sumie wartosci wlasne

# dwie skladowe by wystarczyly

# e) obliczyc kierunki glowne oraz skladowe glowne

pc2$scores     # skladowe
pc2$loadings   # kierunki (ladunki)

# f) znalezc miasto o najwiekszej wartosci pierwszej skladowej glownej - 
#    o czym swiadczy duza wartosc pierwszej skladowej glownej?

which.max(pc2$scores[,1])   # Manila 
pc2$loadings   

# pierwsza skladowa glowna = 0.485*Work - 0.618*Price - 0.619*Salary = 
# = 0.485*Work - 0.618*(Price + Salary)
# tak wiec, skoro pierwsza skladowa glowna jest duza, oznacza to, ze w tym
# miescie jest duzo pracy, a mala jest pensja i koszty utrzymania

m["Manila",]

#        Work Price Salary
# Manila 2268    40      4


# 8.3

library("faraway")
data(meatspec)
attach(meatspec)
head(meatspec)

# a)

tren <- meatspec[1:172,]
test <- meatspec[173:215,]

l <- lm(fat~.,data=tren)
summary(l)

co <- l$coefficients
sum <- 0
te <- as.matrix(test)

for(i in 1:nrow(test)){
   a <- (te[i,101] - c(1,te[i,-101]) %*% matrix(co,ncol=1))^2
   sum <- sum + a
}

rmse <- sqrt(sum/nrow(te))
rmse    # [1,] 3.814

# b)

a <- step(l,direction="backward",k=log(nrow(tren)))

# fat ~ V1 + V2 + V4 + V5 + V6 + V7 + V10 + V12 + V13 + V14 + V15 + 
#    V19 + V21 + V22 + V23 + V24 + V26 + V28 + V29 + V32 + V34 + 
#    V39 + V40 + V41 + V42 + V43 + V44 + V45 + V46 + V50 + V51 + 
#    V52 + V53 + V55 + V56 + V59 + V60 + V63 + V64 + V66 + V68 + 
#    V69 + V72 + V73 + V74 + V77 + V78 + V79 + V80 + V82 + V83 + 
#    V85 + V87 + V92 + V94 + V97 + V99

l2 <- lm(a,data=tren)

# rmse obliczone troche inna (prostsza) metoda

rmse <- sqrt(mean((predict(l2,test)-test$fat)^2))  # [1] 4.082035

# c)

m <- scale(meatspec[,-101])

pca <- princomp(~., cor=FALSE, data=as.data.frame(m)) 

# d)

as.matrix(round(pca$loadings[,1],digits=2))
# Pierwsze skladowa glowna jest suma wszystkich zmiennych*(-0.1)

# e)

summary(pca)
plot(pca)
plot(1:10,pca$sdev[1:10],type="l")

# wybieramy piec skladowych - tak na oko w sumie

tren2 <- as.data.frame(cbind(pca$scores[1:172,1:5],fat=tren$fat))
test2 <- as.data.frame(cbind(pca$scores[173:215,1:5],fat=test$fat))

l2 <- lm(fat~.,data=tren2)
summary(l2)

rmse <- sqrt(mean((predict(l2,test2) - test2$fat)^2)) # [1] 3.212967

# f)

pc <- princomp(~.,cor=FALSE,data=as.data.frame(scale(meatspec[-101])))
b <- pc$scores[1:130,]
t <- pc$scores[131:172,]
y <- meatspec$fat[1:130]
y2 <- meatspec$fat[131:172]

tr <- as.data.frame(cbind(fat=y,b))
wal <- as.data.frame(cbind(fat=y2,t))

rmse <- numeric(100)

for(i in 2:101){
   l <- lm(fat~.,data=tr[,1:i])
   rmse[i-1] <- sqrt(mean((predict(l,wal) - wal$fat)^2))
}

which(rmse==min(rmse))  # 17

# Wybieramy model skladajacy sie z pierwszych 17 skladowych glownych.

tes <- as.data.frame(cbind(fat=meatspec$fat[173:215],pc$scores[173:215,1:17]))
l <- lm(fat~.,data=tr[,1:18])
rmse <- sqrt(mean((predict(l,tes) - tes$fat)^2))  # [1] 2.241767
