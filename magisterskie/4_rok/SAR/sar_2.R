# 2.1

x <- seq(0,10,by=0.1)
lx <- length(x)
eps <- rnorm(lx,0,3)

y <- x + eps

# wykres rozproszenia (xi,yi)

plot(x,y)

# a) wspolczynnik korelacji probkowej korzystajac z def i funkcji cor() 

cor(x,y)    # [1] 0.7207092

sdx <- sd(x); sdy <- sd(y)
c <- cov(x,y)

c/(sdx*sdy)   # [1] 0.7207092   - zgadza sie :D

# b) wspolczynniki prostej z def oraz lm()

l <- lm(y~x)
names(l)
l$coefficients    # (Intercept)           x 
                  #  -0.3702927   1.0205711 

sigma <- 3               # wzor dla przypadku, gdy sigma jest taka sama 
s <- 1*lx                # dla wszystkich eps  
sx <- sum(x)
sy <- sum(y)
sxx <- sum(x^2)
syy <- sum(y^2)
sxy <- sum(x*y)
delta <- s*sxx-sx^2

a <- (s*sxy - sx*sy)/delta
b <- (sxx*sy - sx*sxy)/delta
a           # [1] 1.020571
b           # [1] -0.3702927       - zgadza siê :D

# c) naniesc MNK na wykres rozproszenia

plot(x,y)
abline(l,col="red")

# d) powtorzyc procedure z a) - c) dla sigma=0.5 oraz sigma=5

# sigma = 0.5
x <- seq(0,10,by=0.1)
lx <- length(x)
eps <- rnorm(lx,0,0.5)
y <- x + eps

cor(x,y)    # [1] 0.9885259

l2 <- lm(y~x)
l2$coefficients   # (Intercept)           x 
                  # -0.01835486  1.00558689 

plot(x,y)
abline(l2,col="red")

# sigma = 5

x <- seq(0,10,by=0.1)
lx <- length(x)
eps <- rnorm(lx,0,5)
y <- x + eps

cor(x,y)      # [1] 0.4514702 

l3 <- lm(y~x)
l3$coefficients   # (Intercept)           x 
                  #  -0.1497354   0.8476754 

plot(x,y)
abline(l3,col="red")

# 2.2

# wczytac i wyswietlic zbior anscombe_quartet.txt

a <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                anscombe_quartet.txt",header=TRUE)
head(a)

# a) do kazdej z czterech par zmiennych dopasowac prosta MNK

l1 <- lm(a$Y1~a$X1)
l2 <- lm(a$Y2~a$X2)
l3 <- lm(a$Y3~a$X3)
l4 <- lm(a$Y4~a$X4)

# b) porownac otrzymane wspolczynniki dopasowanych prostych MNK, 
#    wspolczynniki R^2 i wspolczynniki korelacji

# wspolczynniki prostej

l1$coefficients      # (Intercept)        a$X1 
                     #   3.0000909   0.5000909 
l2$coefficients      # (Intercept)        a$X2 
                     #    3.000909    0.500000 
l3$coefficients      # (Intercept)        a$X3 
                     #   3.0024545   0.4997273 
l4$coefficients      # (Intercept)        a$X4 
                     #   3.0017273   0.4999091 

# R^2

summary(l1)$r.squared    # [1] 0.6665425
summary(l2)$r.squared    # [1] 0.666242
summary(l3)$r.squared    # [1] 0.666324
summary(l4)$r.squared    # [1] 0.6667073

# wspolczynniki korelacji

cor(a$X1,a$Y1)       # [1] 0.8164205
cor(a$X2,a$Y2)       # [1] 0.8162365
cor(a$X3,a$Y3)       # [1] 0.8162867
cor(a$X4,a$Y4)       # [1] 0.8165214

# c) wykresy rozrzutu (xi,yi), w ktorym przypadku mozna mowic o przyblizonej 
#    zaleznosci liniowej y od x

par(mfrow=c(2,2))

plot(a$X1,a$Y1)
abline(l1,col="red")

plot(a$X2,a$Y2)
abline(l2,col="blue")

plot(a$X3,a$Y3)
abline(l3,col="green")

plot(a$X4,a$Y4)
abline(l4,col="orange")

par(mfrow=c(1,1))

# 2.3

# zbior danych hills dotyczacych biegow przelajowych, zawiera trzy zmienne:
# time - rekordodwy czas pokonania trasy [min]
# dist - dlugosc trasy [mile]
# climb - calkowita roznica poziomow do pokonania na trasie [stopy]

library("MASS")
data(hills)
h <- hills
head(h)

# a) porownac wykresy rozproszenia time od dist i time os climb, obliczyc
#    wspolczynniki korelacji time i dist oraz time i climb

par(mfrow=c(1,2))

plot(h$dist,h$time)
plot(h$climb,h$time)

cor(h$dist,h$time)         # [1] 0.9195892
cor(h$climb,h$time)        # [1] 0.8052392

# b) dopasowac proste MNK, naniesc je na wykresy rozproszenia, obliczyc R^2
#    poslugujac sie dwiema metodami: korzystajac z summary() i z definicji

ld <- lm(h$time~h$dist)
lc <- lm(h$time~h$climb)

plot(h$dist,h$time)
abline(ld,col="red")
plot(h$climb,h$time)
abline(lc,col="blue")

# R^2

summary(ld)$r.squared     # [1] 0.8456444
summary(lc)$r.squared     # [1] 0.6484101

sse <- sum(ld$residuals^2)
sst <- sum((h$time-mean(h$time))^2)
1-sse/sst       # [1] 0.8456444

sse <- sum(lc$residuals^2)
sst <- sum((h$time-mean(h$time))^2)
1-sse/sst       # [1] 0.6484101        - zgadza sie :D

# c) jaki rekordowy czas polonania trasy o dlugosci 15 mil przewidzimy 
#    poslugujac sie prosta mnk?

co <- ld$coefficients
co[1] + co[2]*15    # 120.1161 

# 2.4

# zbior danych trees - rozmiary dotyczace drzew:
# Girth - obwod drzewa na poziomie piersi [cale]
# Height - wysokosc
# Volume - objetosc

library("base")
data(trees)
t <- trees
head(t)

# a) sporzadzic wykresy rozproszenia objetosci wzgledem obwodu i objetosci 
#    wzgledem wysokosci


plot(t$Girth,t$Volume)
plot(t$Height,t$Volume)    # widac, ze zaleznosc jest silniejsza w przypadku 
                           # Girth

# b) dopasowac modele liniowe, naniesc proste na wykresy rozproszenia

lg <- lm(t$Volume~t$Girth)
lh <- lm(t$Volume~t$Height)

plot(t$Girth,t$Volume)
abline(lg,col="red")
plot(t$Height,t$Volume)
abline(lh,col="blue")

# c) porownac R^2 dla tych modeli

summary(lg)$r.squared    # [1] 0.9353199
summary(lh)$r.squared    # [1] 0.3579026

# d) przy zalozeniu prawdziwosci modelu liniowego, jaki jest wynik testu na 
#    istnienie zaleznosci miedzy tymi zmiennymi? Przyjac poziom istotnoci 
#    0.05

# H: beta = 0
# K: beta != 0
# sprawdzamy to w summary() - nie dla Interceptu, tylko dla tej drugiej
#                             zmiennej

summary(lg)       # p-value < 2e-16 *** - male, wiec odrzucamy hipoteze, 
                  # czyli zaleznosc miedzy tymi zmiennymi istnieje
summary(lh)       # p-value = 0.000378 *** - male, wiec odrzucamy hipoteze,
                  # czyli zaleznosc miedzy tymi zmiennymi istnieje

# e) ile wynosi oszacowanie wariancji objetosci drzewa w modelu liniowym 
#    opisujacym zaleznosc objetosci od obwodu?

# korzystajac z summary()

summary(lg)$sigma^2            # [1] 18.0794

# z definicji

sse <- sum(lg$residuals^2)
sse/(length(t$Volume)-2)       # [1] 18.0794  -  zgadza sie!

# f) na poziomie ufnosci 0.95 podac przedzial ufnosci dla wspolczynnika 
#    kierunkowego beta1 w tym modelu

alfa <- 0.05
beta1 <- lg$coefficients[2]
se <- summary(lg)$coef[2,2]
q <- qt(1-alfa/2, length(t$Volume)-2)

l <- beta1 - q*se 
p <- beta1 + q*se

l; p       # [4.559914,5.571799]

# g) dla obu modeli sporzadzic wykresy rozproszenia rezyduow

# wzgledem numeru porzadkowego

plot(lg$residuals)
plot(lh$residuals)

# wzgledem zmiennej objasniajacej

plot(t$Girth,lg$residuals)
plot(t$Height,lg$residuals)

# wzgledem wartosci dopasowanych

plot(lg,1)
plot(lh,1)    # powinna byc chmura punktow, a na obu wykresach widac
              # zaleznosc :(

# inne

plot(lg,2)
plot(lh,2)    # wykres kwantylowy - punkty powinny sie tu ukladac na prostej

# h) na podstawie powyzszej analizy zaproponowac model wyzszego stopnia 
#    opisujacy zaleznosc objetosci od obwodu. Dopasowac go i porownac 
#    oszacowanie wariancji objetosci drzewa w tym modelu z wartoscia 
#    obliczona w punkcie e)

# proponuje kwadratowy model

lg2 <- lm(t$Volume~I(t$Girth^2))

plot(t$Girth^2,t$Volume)
abline(lg2,col="red")

plot(lg2,1)    # widac, ze jest troche lepiej

summary(lg2)$sigma^2    # [1] 11.35583 - wyszla mniejsza wariancja :D

# i) poslugujac sie dopasowanym modelem wyznaczyc przewidywana wartosc 
#    sredniej objetosci drzewa o obwodzie rownym 15 cali

co <- lg2$coefficients
co[1] + co[2]*15^2         # 37.40884  - pamietac tu o kwadracie!

coo <- lg$coefficients
coo[1] + coo[2]*15         # 39.04439  - pierwszy model przewidywalby nam 
                           #             troche wieksza objetosc

































