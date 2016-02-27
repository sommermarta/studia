# 3.1

# zbior airopllution.txt opisuje zwiazek pomiedzy zanieczyszczeniem  
# powietrza i smiertelnoscia w 60 miastach amerykanskich:
# Mortality - liczba zgonow na 100 000 mieszkancow
# NOx - stezenie tlenku azotanu

a <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                airpollution.txt",header=TRUE)
head(a)

# a) dopasowac model liniowy Mortality od NOx. Podac wspolczynnik nachylenia 
#    prostej oraz jego blad standardowy. Czy dopasowany model dobrze opisuje
#    dane?

m <- a$Mortality
n <- a$NOx

l <- lm(m~n)
plot(n,m)
abline(l)

l$coefficients[2]                # -0.1042702 
summary(l)$coefficients[2,2]     # [1] 0.1757082

summary(l)        # p-value = 0.555 > 0.05, czyli przyjmujemy hipoteze, 
                  # czyli ta zmienna jest nieistotna (nie ma zaleznosci 
                  # miedzy Mortality a NOx)

# b) dopasowac model liniowy Mortality od log(NOx). Podac wspolczynnik 
#    nachylenia prostej i jego blad standardowy. Czy model dobrze opisuje 
#    dane?

l2 <- lm(m~I(log(n)))
 
plot(log(n),m)
abline(l2,col="red")

l2$coefficients[2]              # 15.09896 
summary(l2)$coefficients[2,2]   # [1] 6.41871
 
summary(l2)      # p-value = 0.0221 *, czyli zmienna jest istotna 
                 # R^2 = 0.0871 - male, czyli dopasowanie nie jest najlepsze

plot(l2,1)

# c) w modelu z punktu b) znalezc obserwacje o duzych rezyduach 
#    studentyzowanych, sporzadzic nowy model pomijajac te obserwacje,
#    porownac wartosci R^2 dla tych dwoch modeli

rs <- rstudent(l2)   # obserwacje majace modul tego wiekszy niz dwa beda 
                     # odstajace
w <- which(abs(rs)>2)     # 29 37 47 49 

l3 <- lm(m~I(log(n)), subset=-w)

plot(log(n)[-w],m[-w])
abline(l3,col="red")

plot(l3,1)

summary(l3)$r.squared    # [1] 0.3220114 - widac, ze R^2 jest tu duzo lepsze

# 3.2

# zbior danych phila.txt zebrano informacje dotyczace cen domow (HousePrice)
# polozenych w okolicach Filadelfii i innych ich cech (wskaznik 
# przestepczosci w okolicy danego domu - CrimeRate)

# a) wczytac zbior i zauwazyc, ze brakuje czesci danych

p <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                phila.txt",header=TRUE)
head(p)

hp <- p$HousePrice
cr <- p$CrimeRate

sum(is.na(hp))      # [1] 0
sum(is.na(cr))      # [1] 11   - tu brakuje jedenastu danych

# gdy brak danych, to lm() stosuje na.omit()

# b) zleznosc ceny od wskaznika przestepczosci, sporzadzic wykres 
#    rozproszenia i dopasowac model liniowy, zidentyfikowac potencjalna 
#    obserwacje odstajaca i wplywowa, usunac ja ze zbioru i ponownie
#    dopasowac model, czy nowy model dobrze opisuje dane?

l <- lm(hp~cr)
plot(cr,hp)
abline(l,col="red")

plot(l,1)
plot(l,2)

which(cr>300)   # szukam obserwacji odstajacej korzystajac z wykresu 
                # rozproszenia
                # [1] 70 - ta jest odstajaca

l2 <- lm(hp~cr,subset=-c(70))
plot(cr[-70],hp[-70])
abline(l2,col="red")

plot(l2,1)      # ju¿ troche lepiej bez odstajacej obserwacji
plot(l2,2)

summary(l)
summary(l2)     # w obu przypadkach zmienna jest istotna - w drugim jednak 
                # jest wieksza zaleznosc

summary(l)$r.squared    # [1] 0.06248047
summary(l2)$r.squared   # [1] 0.1842288 - R^2 tez jest tu wieksze :D

# 3.3

# zbior cellular.txt zawiera informacje dotyczace liczby czytelnikow pewnego 
# czasopisma (zmienna Subscribers)

c <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                 cellular.txt",header=TRUE)
head(c)

# a) sporzadzic wykres liczby czytelnikow od funkcji czasu (zmienna Period),
#    dopasowac model liniowy i przeprowadzic jego diagnostyke, zwrocic uwage
#    na fakt, ze beta1 jest istotny mimo, ze model jest zle dopasowany

s <- c$Subscribers
p <- c$Period
l <- lm(s~p)

plot(p,s)
abline(l,col="red")

summary(l)

# b) wybrac model najlepszy sposrod nastepujacych modeli alternatywnych:

# log(s) ~ p

l2 <- lm(I(log(s))~p)
plot(p,log(s))
abline(l2,col="red")
summary(l2)

# sqrt(s) ~ p

l3 <- lm(I(sqrt(s))~p)
plot(p,sqrt(s))
abline(l3,col="blue")
summary(l3)

# s^(1/4) ~ p

l4 <- lm(I(s^(1/4))~p)
plot(p,s^(1/4))
abline(l4,col="brown")
summary(l4)

plot(l2,1)
plot(l3,1)
plot(l4,1)   # ostatni model jest chyba najlepszy

# c) uzywajac metody Boxa-Coxa, wyznaczyc przeksztalcenie g() zmiennej s 
#    dajace najlepszy model liniowy g(s)~p

library("MASS")
boxcox(s~p,data=c,lambda=seq(-0.5,0.5,length=20))  # widac, ze najlepsze 
                                                   # bedzie cos okolo 0.2
l5 <- lm(I(s^(0.2))~p)
plot(p,s^(0.2))
abline(l5,col="red")
plot(l5,1)

summary(l5)

# 3.4

# Zbior strongx.txt dotyczy sytuacji eksperymentalnej w fizyce czastek 
# elementarnych.
# crossx - przekroj czynny
# energy - odwrotnosc energii
# momentum - ped czastek
# Dla kazdej wartosci pedu powtorzono eksperyment wielokrotnie, co pozwolilo
# oszacowac odchylenie standardowe zmiennej corssx
# sd - odchylenie statdardowe

# a) wyznaczyc proste MNK i MNWK corssx od energy; wykres rozproszenia; 
#    ktora lepiej dopasowana?; dlaczego MNWK przybliza lepiej punkty 
#    odpowiadajace niskim energiom?

s <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                strongx.txt",header=TRUE)
head(s)

c <- s$crossx
e <- s$energy
sd <- s$sd

mnk <- lm(c~e)
mnwk <- lm(c~e, weights=sd^(-1))  # wagi to 1/sqrt(wariancji)

plot(e,c)
abline(mnk,col="red")
abline(mnwk,col="blue")

summary(mnk)      # -> R^2 = 0.9548
summary(mnwk)     # -> R^2 = 0.9453

# MNWK lepiej przybliza niskie energie, bo dla nich sa wieksze wagi

# b) zaproponowac zmodyfikowany model

plot(mnk,1)
plot(mnwk,1)

mnk2 <- lm(c~I(e^2))
summary(mnk2)     # -> R^2 = 0.9925 

mnwk2 <- lm(c~I(e^2), weights=sd^(-1))
summary(mnwk2)     # -> R^2 = 0.9917 

plot(e^2,c)
abline(mnk2,col="red")
abline(mnwk2,col="blue")   # sa praktycznie identyczne

plot(mnk2,1)
plot(mnwk2,1)

