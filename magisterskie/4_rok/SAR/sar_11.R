# 11.1

a <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                coagulat.txt",header=TRUE)
attach(a)
head(a)

# a)

sr <- tapply(coag,diet,mean)
stripchart(coag~diet,pch=20,vertical=T,main="Wykres srednich w grupach")
lines(1:4,sr,pch=20,type="b",cex=2)

# b)
# wykresy kwantylowe (dla kazdej grupy oddzielnie):

par(mfrow=c(2,2))
tapply(coag,diet,qqnorm)

# wykresy skrzynkowe

plot(diet,coag) 

# test shapiro-wilka (test normalnosci)

shapiro.test(coag[diet=="dietA"])
# Shapiro-Wilk normality test
# 
# data:  coag[diet == "dietA"]
# W = 0.9497, p-value = 0.7143

# duze p-value, to bierzemy hipoteze, czyli jest normalny

shapiro.test(coag[diet=="dietB"])
shapiro.test(coag[diet=="dietC"])
shapiro.test(coag[diet=="dietD"]) # wszystkie sa normalne

# test bartleta (test na rownosc wariancji)

bartlett.test(coag~diet) 

# Bartlett test of homogeneity of variances
# 
# data:  coag by diet
# Bartlett's K-squared = 1.668, df = 3, p-value = 0.6441

# p-value duze, to bierzemy hipoteze, to wariancja stala

# test levene'a

g <- lm(coag~diet)
rez <- lm(abs(g$residuals)~diet)
summary(rez) # p-value: 0.5604 -> duze, to bierzemy hipoteze, to stala
             #                    wariancja

# c)

g <- lm(coag~diet)
summary(g)  # male p-value F, wiec odrzucamy hipoteze, wiec srednie w 
            # grupach istotnie sie roznia
model.matrix(g) # wyswietli macierz eksperymentu

#         (Intercept) dietdietB dietdietC dietdietD
# 1            1         0         0         0
# 2            1         0         0         0
# 3            1         0         0         0
# 4            1         0         0         0
# 5            1         1         0         0
# 6            1         1         0         0
# 7            1         1         0         0
# 8            1         1         0         0
# 9            1         1         0         0
# 10           1         1         0         0
# 11           1         0         1         0
# 12           1         0         1         0
# 13           1         0         1         0
# 14           1         0         1         0
# 15           1         0         1         0
# 16           1         0         1         0
# 17           1         0         0         1
# 18           1         0         0         1
# 19           1         0         0         1
# 20           1         0         0         1
# 21           1         0         0         1
# 22           1         0         0         1
# 23           1         0         0         1
# 24           1         0         0         1

diet <- relevel(diet,ref="dietB") # jesli chcemy zeby na przyklad dietB
                                  # bylo srednia odniesienia, a nie dietA
summary(g)

#                 Estimate Std. Error t value Pr(>|t|)    
#    (Intercept) 6.100e+01  1.183e+00  51.554  < 2e-16 ***
#    dietdietB   5.000e+00  1.528e+00   3.273 0.003803 ** 
#    dietdietC   7.000e+00  1.528e+00   4.583 0.000181 ***
#    dietdietD   2.991e-15  1.449e+00   0.000 1.000000  

# Zatem dietA jest poziomem odniesienia.
# Srednia na poziomie dietA: 61
# Srednia na poziomie dietB: 61+5
# Srednia na poziomie dietC: 61+7
# Srednia na poziomie dietD: 61+0

# Wykres kwantylowy rezyduow i wykres rozproszenia rezyduow wzgledem
# wartosci dopasowanych:

par(mfrow=c(1,2))
plot(g,which=1:2)


# d)

# PROCEDURA BONFERRONI'EGO: 

# za poziom istotnosci pojedynczego testu przyjmujemy alfa/k*, gdzie k*
# to liczba par, ktore porownujemy. UWAGA: Tak dobrany poziom
# istotnosci jest mniejszy od zadanego alfa; ponadto dla duzych k* 
# procedura ta jest bezuzyteczna, bo praktycznie nigdy nie odrzuca hipotezy 
# zerowej.

pairwise.t.test(coag, diet, p.adjust.method="bonferroni")  

#         dietA   dietB   dietC  
# dietB 0.02282 -       -      
# dietC 0.00108 0.95266 -      
# dietD 1.00000 0.00518 0.00014

# miedzy dietA i dietB jest roznica, miedzy dietA i dietD nie ma
# tam gdziem male p-value, tam jest roznica

# PROCEDURA TUKEY'A: 

# poziom istotnosci oparty jest na rozkladzie maksymalnej roznicy pomiedzy
# srednimi (tak zwanym "studentyzowanym rozkladzie rozstepu" dla proby z 
# rozkladu normalnego).

a1 <- aov(coag~diet)  # inaczej zrobiona anova - bo tukey z takiej korzysta
TukeyHSD(a1)   

# $diet
#             diff         lwr       upr     p adj
# dietB-dietA    5   0.7245544  9.275446 0.0183283
# dietC-dietA    7   2.7245544 11.275446 0.0009577
# dietD-dietA    0  -4.0560438  4.056044 1.0000000
# dietC-dietB    2  -1.8240748  5.824075 0.4766005
# dietD-dietB   -5  -8.5770944 -1.422906 0.0044114
# dietD-dietC   -7 -10.5770944 -3.422906 0.0001268

# ostatnia kolumna polazuje, czy sa roznice miedzy poziomami -> miedzy 
# A i B jest, miedzy D i A nie ma...

# 11.2

pier <- c(204,200,198,204)
drug <- c(197,205,213,209)
trze <- c(190,208,202,210)

gat <- factor(rep(1:3,each=4))   # zeby wszystko dzialalo potrzebna jest
                                 # zmienna typu factor!
wytrz <- c(pier,drug,trze)
dane <- cbind(wytrz,gat)

sr <- tapply(wytrz,gat,mean)
stripchart(wytrz~gat,pch=20,vertical=T,main="Wykres srednich w grupach")
lines(1:3,sr,pch=20,type="b",cex=2)

par(mfrow=c(2,2))
tapply(wytrz,gat,qqnorm)

par(mfrow=c(1,1))
plot(gat,wytrz) 

shapiro.test(pier) 
shapiro.test(drug)
shapiro.test(trze) # p-value duze, wszystkie sa normalne

bartlett.test(wytrz~gat) # p-value duze, wariancja jest wiec jednorodna

g <- lm(wytrz~gat)
rez <- lm(abs(g$residuals)~gat)
summary(rez)  # p-value: 0.3274 - duze, wiec wariancja jednorodna

# zalozenia anovy sa spelnione

summary(g)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  201.500      3.375  59.708 5.22e-13 ***
# gat2           4.500      4.773   0.943    0.370    
# gat3           1.000      4.773   0.210    0.839    
# ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.749 on 9 degrees of freedom
# Multiple R-squared:  0.09824,   Adjusted R-squared:  -0.1022 
# F-statistic: 0.4902 on 2 and 9 DF,  p-value: 0.6279

# p-value duze, wiec gatunek nie wplywa na wytrzymalosc


# 11.3

p <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/pszen.txt",header=TRUE)
p
attach(p)

# a)

sr <- tapply(plon,azot,mean)
stripchart(plon~azot,pch=20,vertical=T,main="Wykres srednich w grupach")
lines(1:4,sr,pch=20,type="b",cex=2)

par(mfrow=c(2,2))
tapply(plon,azot,qqnorm)

par(mfrow=c(1,1))
plot(azot,plon) 

shapiro.test(p$plon[p$azot=="dawka1"]) 
shapiro.test(p$plon[p$azot=="dawka2"]) 
shapiro.test(p$plon[p$azot=="dawka3"])  
shapiro.test(p$plon[p$azot=="dawka4"]) # p-value duze, wszystkie sa normalne 

bartlett.test(plon~azot) # p-value duze, wariancja jest wiec jednorodna

g <- lm(plon~azot)
rez <- lm(abs(g$residuals)~azot)
summary(rez)  # p-value duze, wiec wariancja jednorodna

# zalozenia anovy sa spelnione

summary(g)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  70.1750     1.3945  50.322   <2e-16 ***
# azotdawka2    0.1625     1.9721   0.082   0.9349    
# azotdawka3    1.8500     1.9721   0.938   0.3562    
# azotdawka4    4.0875     1.9721   2.073   0.0475 *  
#    ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.944 on 28 degrees of freedom
# Multiple R-squared:  0.1662,   Adjusted R-squared:  0.07687 
# F-statistic:  1.86 on 3 and 28 DF,  p-value: 0.1592

# p-value duze, nie ma roznic miedzy azotem

# b)

komb <- azot:metoda  # wszystkie kombinacje azotu i dawki

sr <- tapply(plon,komb,mean)
stripchart(plon~komb,pch=1,vertical=T,main="Wykres srednich w grupach")
lines(1:8,sr,pch=20,type="b",cex=2)

par(mfrow=c(2,2))
tapply(plon,komb,qqnorm)

par(mfrow=c(1,1))
plot(komb,plon) 

shapiro.test(p$plon[p$azot=="dawka1" & p$metoda=="met1"]) 
shapiro.test(p$plon[p$azot=="dawka2" & p$metoda=="met1"]) 
shapiro.test(p$plon[p$azot=="dawka3" & p$metoda=="met1"])  
shapiro.test(p$plon[p$azot=="dawka4" & p$metoda=="met1"]) 
shapiro.test(p$plon[p$azot=="dawka1" & p$metoda=="met2"]) 
shapiro.test(p$plon[p$azot=="dawka2" & p$metoda=="met2"]) 
shapiro.test(p$plon[p$azot=="dawka3" & p$metoda=="met2"])  
shapiro.test(p$plon[p$azot=="dawka4" & p$metoda=="met2"])

# p-value wszedzie duze, wiec wszystkie sa normalne 

bartlett.test(plon~komb) # p-value duze, wariancja jest wiec jednorodna

g <- lm(plon~komb)
rez <- lm(abs(g$residuals)~komb)
summary(rez)  # p-value duze, wiec wariancja jednorodna

# wszystkie zalozenia anovy sa spelnione

# wykresy interakcji (jesli rownolegle, to brak interakcji):

par(mfrow=c(1,2))
interaction.plot(azot,metoda,plon)  
interaction.plot(metoda,azot,plon)

l <- lm(plon~azot*metoda)
# l <- lm(plon~azot+metoda+azot:metoda) - inaczej tak mozna zapisac

anova(l) # - duze p-value przy azot:metoda, to nie ma interakcji

# Response: plon
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# azot         3  86.83   28.94  8.3674 0.0005547 ***
# metoda       1 341.91  341.91 98.8421 5.507e-10 ***
# azot:metoda  3  10.68    3.56  1.0288 0.3974760    
# Residuals   24  83.02    3.46     

# model bez interakcji:

l2 <- lm(plon~azot+metoda) 
anova(l2)  # oba czynniki istotne, bo p-value testu F male

#           Df Sum Sq Mean Sq F value    Pr(>F)    
# azot       3  86.83   28.94  8.3407 0.0004372 ***
# metoda     1 341.91  341.91 98.5269 1.664e-10 ***
# Residuals 27  93.70    3.47                      

anova(l2,l) # p-value = 0.3975 -> przyjmujemy hipoteze, ze mniejszy model 
            #                     jest lepszy

summary(l2)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  66.9062     0.7364  90.861  < 2e-16 ***
# azotdawka2    0.1625     0.9314   0.174 0.862803    
# azotdawka3    1.8500     0.9314   1.986 0.057249 .  
# azotdawka4    4.0875     0.9314   4.388 0.000157 ***
# metodamet2    6.5375     0.6586   9.926 1.66e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.863 on 27 degrees of freedom
# Multiple R-squared:  0.8207,   Adjusted R-squared:  0.7941 
# F-statistic: 30.89 on 4 and 27 DF,  p-value: 1.016e-09

a <- aov(plon~azot+metoda)
TukeyHSD(a) 

# $azot
#                 diff        lwr      upr     p adj
# dawka2-dawka1 0.1625 -2.3864117 2.711412 0.9980649
# dawka3-dawka1 1.8500 -0.6989117 4.398912 0.2180285
# dawka4-dawka1 4.0875  1.5385883 6.636412 0.0008568
# dawka3-dawka2 1.6875 -0.8614117 4.236412 0.2899201
# dawka4-dawka2 3.9250  1.3760883 6.473912 0.0013528
# dawka4-dawka3 2.2375 -0.3114117 4.786412 0.1007085
# 
# $metoda
# diff      lwr      upr p adj
# met2-met1 6.5375 5.186126 7.888874     0

# czyli roznica jest miedzy nawodnieniem i miedzy dawka 4 a 2...


# 11.4

t <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                trucizny.txt",header=TRUE)
head(t)
attach(t)

# a)

interaction.plot(trucizna,kuracja,wyczas)
interaction.plot(kuracja,trucizna,wyczas)

a <- aov(wyczas~trucizna*kuracja)
summary(a) # p-value duze, to interakcja nieistotna

# b)

komb <- trucizna:kuracja  

sr <- tapply(wyczas,komb,mean)
stripchart(wyczas~komb,pch=1,vertical=T,main="Wykres srednich w grupach")
lines(1:12,sr,pch=20,type="b",cex=2)

par(mfrow=c(2,2))
tapply(wyczas,komb,qqnorm)

par(mfrow=c(1,1))
plot(komb,wyczas) 

naz <- unique(trucizna)
kur <- unique(kuracja)
sh <- matrix(0,nrow=3,ncol=4)

for(i in 1:3){
   for(j in 1:4){
      sh[i,j] <- shapiro.test(t$wyczas[t$trucizna==naz[i] & 
                                          t$kuracja==kur[j]])$p.value  
   }
}

sum(sh <= 0.05)  # 0 -> test na normalnosc zdany :D

bartlett.test(wyczas~komb) # p-value male, wariancja nie jest wiec jednorodna

g <- lm(wyczas~komb)
rez <- lm(abs(g$residuals)~komb)
summary(rez)  # p-value male, wiec wariancja nie jest jednorodna

# nie wszystkie zalozenia anovy sa spelnione...

# rozklad rezyduow:

l <- lm(wyczas~trucizna*kuracja)
summary(l)
plot(l,which=1)

# bardzo wyraznie widoczna niestala wariancja rezyduow

# inny model:

l2 <- lm(log(wyczas)~trucizna*kuracja)
summary(l2)
plot(l2,which=1)

# jest troche lepszy, ale mozna by go jeszcze poprawic.

library(MASS)
boxcox(l,plotit=T)

l3 <- lm((wyczas)^(-1)~trucizna*kuracja)
summary(l3)
plot(l3,which=1)

# jest to o wiele lepszy model

# c)

anova(l3)

# interakcja nieistotna

l3_b <- lm((wyczas)^(-1)~trucizna+kuracja)
summary(l3_b)
anova(l3_b)  # oba czynniki istotne

interaction.plot(trucizna,kuracja,wyczas^(-1))
interaction.plot(kuracja,trucizna,wyczas^(-1))

# porownania wielokrotne:

pairwise.t.test(wyczas,trucizna,p.adjust.method="bonferroni")
pairwise.t.test(wyczas^(-1),trucizna,p.adjust.method="bonferroni")
TukeyHSD(aov(l3_b))

# 11.5

n <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                nsf2.txt",header=TRUE)
head(n)
attach(n)

# a)

# na kazdym ustalonym poziomie dwoch czynnikow mamy tylko jedna obserwacje, 
# wiec nie mozemy wykonac testow istotnosci w modelu z interakcjami,
# bo pojawia sie dzielenie przez zero:

a <- aov(zarobek~rasa*zawod,data=n)
summary(a)

#             Df  Sum Sq Mean Sq
# rasa         1  94.531  94.531
# zawod        3 141.844  47.281
# rasa:zawod   3  14.594   4.865

# -> brak testu F 

# obecnosc interakcji musimy sprawdzic metoda graficzna:

par(mfrow=c(1,2))
interaction.plot(rasa,zawod,zarobek)
interaction.plot(zawod,rasa,zarobek)

# -> stwierdzamy brak interakcji

l <- lm(zarobek~rasa+zawod,data=n)
summary(l)
l1 <- aov(zarobek~rasa+zawod,data=n)
summary(l1)

#             Df Sum Sq Mean Sq F value  Pr(>F)   
# rasa         1  81.28   81.28   85.75 0.00267 **
# zawod        3 169.09   56.36   59.46 0.00359 **
# Residuals    3   2.84    0.95

# w tym modelu testowanie wplywu poszczegolnych czynnikow jest juz mozliwe

# -> stwierdzamy, ze jest efekt glowny dla obu czynnikow


# dodajemy do danych jedna obserwacje:

w <- data.frame(rasa=c("biala","zolta"),zawod="inz.przem",zarobek=c(59.0,65.0))
nn <- rbind(n,w)

interaction.plot(nn$rasa,nn$zawod,nn$zarobek)
interaction.plot(nn$zawod,nn$rasa,nn$zarobek)

# widoczna jest interakcja

a <- aov(zarobek~rasa+zawod,data=nn)
summary(a)

#             Df  Sum Sq Mean Sq F value Pr(>F)
# rasa         1  46.225  46.225  2.2855 0.2051
# zawod        4 216.100  54.025  2.6712 0.1822
# Residuals    4  80.900  20.225 

# -> brak efektu glownego dla obu czynnikow

# WNIOSEK: nie uwzgledniajac interakcji (ktore sa rzeczywiscie obecne)
# mozemy dojsc do blednych wnioskow, ze czynniki nie maja wplywu na 
# srednia odpowiedz.
