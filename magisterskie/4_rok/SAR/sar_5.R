# 5.1

Data <- read.csv2("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                  ConcreteData.csv")
head(Data)
names(Data)
dim(Data)    # [1] 1030    9

set.seed(1)  # ¿eby wartoœci sample() by³y ustalone

n <- nrow(Data)
s <- sample(1:n,500)

y <- as.numeric(Data[,9]) 
x <- as.matrix(Data[,1:8])

xx <- x[s,]
yy <- y[s]

l <- lm(yy~xx)
summary(l)

xt <- x[-s,]
yt <- y[-s]

# teraz bêdê obliczaæ b³¹d predykcji

c <- l$coefficients
beta <- as.numeric(c)

mean( (cbind(1,xt) %*% beta - yt)^2 )   # [1] 110.4961 

# Powtórz powy¿sze dla ró¿nych podzia³ów danych na próbê treningow¹ i 
# testow¹. Na tej podstawie oblicz b³¹d predykcji, odchylenie standardowe
# oraz sporz¹dŸ wykres typu boxplot dla b³êdów predykcji.

e <- numeric(100)

for(i in 1:100){       
   
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

mean(e)  # [1] 111.1227

boxplot(e, horizontal=TRUE)


# 5.5

# dane dotycz¹ce smaku sera (zmienna taste)

# H0: mniejszy model jest adekwatny
# H1: wiêkszy model jest adekwatny

ch <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                 cheese.txt",header=TRUE)
head(ch)

l1 <- lm(ch$taste~ch$Acetic)
l2 <- lm(ch$taste~ch$Acetic+ch$H2S+ch$Lactic,data=ch)

summary(l1)
summary(l2)

anova(l1,l2)

# statystyka F: 13.058
# p-value: 0.0001186 *** -> ma³e, czyli odrzucamy H0, czyli wiêkszy model
#                           jest lepszy


# 5.2

s <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                savings.txt",header=T)
head(s)
attach(s)

# a) dopasowaæ model liniowy, zidentyfikowaæ obserwacje o du¿ych 
#    modyfikowanych studentyzowanych rezyduach i du¿ych wp³ywach (h_ii),
#    sporz¹dziæ diagram Cooka, usun¹æ z modelu obserwacjê o najwiêkszej 
#    odleg³oœci Cooka, sprawdziæ, ¿e jest ona wp³ywowa

l <- lm(Savings~.-Country, data=s)
summary(l)    # Multiple R-squared:  0.3385,   Adjusted R-squared:  0.2797 
plot(l,1)

rs <- rstudent(l)
rs[abs(rs) > 2]    #         7        46 
                   # -2.313460  2.853581 

hat <- hatvalues(l)
# tu jest 2*(liczba zmiennych obserwowanych / liczba obserwacji)
hat[hat > 2*5/length(Savings)]   #        21        23        44        49 
                                 # 0.2122362 0.2233127 0.3336981 0.5314551 

cook <- cooks.distance(l)
cook[cook==max(cook)]   #        49 
                        # 0.2680594 

plot(l,4)   # wykres Cooka

ss <- s[-49,]
l2 <- lm(Savings~.-Country, data=ss)
summary(l2)  # Multiple R-squared:  0.3554,   Adjusted R-squared:  0.2968 
plot(l2,1)

# b) 

# Dlaczego dpi jest nieistotna w modelu dopasowanym w punkcie a)? Sporz¹dziæ
# czêœciowe wykresy regresji dla zmiennych dpi i ddpi.

# dpi jest nieistotna, bo p-value testu t jest du¿e dla tej zmiennej, wiêc
# przyjmujemy hipotezê, ¿e zmienna ta jest nieistotna

l1y <- lm(Savings~.-Country-dpi,data=ss)
l2y <- lm(Savings~.-Country-ddpi,data=ss)

l1x <- lm(dpi~.-Country-dpi-Savings,data=ss)
l2x <- lm(ddpi~.-Country-ddpi-Savings,data=ss)

r1y <- l1y$residuals
r1x <- l1x$residuals

r2y <- l2y$residuals
r2x <- l2x$residuals

ll <- lm(r1y~r1x)
lll <- lm(r2y~r2x)

plot(r1x,r1y)
abline(ll,col="red")   # prosta pozioma, to zmienna jest nieistotna (dpi)

plot(r2x,r2y)
abline(lll,col="blue")  # prosta pochy³a, to zmienna jest istotna (ddpi)

# c) wspó³czynnik korelacji sla zmiennych Pop15 i Pop75, porównaæ go ze 
#    wspó³czynnikiem korelacji odpowiednich predyktorów

cor(Pop15,Pop75) # [1] -0.9084751 -> widaæ, ¿e s¹ silnie zale¿ne
summary(l2,cor=TRUE)  # dla Pop15 i Pop75 jest 0.79

# Wspó³czynniki maj¹ dodatni¹ korelacjê. Korelacja miêdzy predyktorami
# i korelacja miêdzy wspó³czynnikami tych predyktorów czêsto maj¹ ró¿ne 
# znaki. Intuicyjnie mo¿emy to wyjaœniæ tak, ¿e dwa ujemnie skorelowane 
# predyktory próbuj¹ wykonaæ tê sam¹ pracê. Im wiêcej pracy zrobi jeden, 
# tym mniej mo¿e zrobiæ drugi i st¹d dodatnia korelacja wspó³czynników. 

# d) wykres czêœciowych reziduów dla zmiennej Pop15

install.packages("faraway")
library("faraway")

summary(l2)  
prplot(l2,1)   # wykres czêœciowych reziduów dla pierwszej zmiennej, która 
               # siê wyœwietla w summary()
# wykres pochy³y, czyli zmienna jest istotna

# Na tym wykresie widaæ dodatkow¹ relacjê w danych - rozdzielenie na dwie 
# grupy. Mo¿e wiêc lepiej inaczej dopasowaæ proste?

# wartosc 35 zostala wybrana na podstawie wykresu

lm(Savings~.-Country, data=ss, subset=Pop15<35)$coef["Pop15"]   # -39
lm(Savings~.-Country, data=ss, subset=Pop15>35)$coef["Pop15"]   # 0.27

# Dwie grupy -> w jednej zale¿noœæ jest ujemna, w drugiej dodatnia!
# Czyli model opieraj¹cy siê na wszystkich danych mo¿e nie byæ poprawny.

# 5.4

# dane dotycz¹ce sprzeda¿y papierosów (zmienna Sales)

ci <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR
                 /DANE/cigconsumption.txt",header=T)
head(ci)
attach(ci)

# a)

l <- lm(Sales~.-State, data=ci)
summary(l)  # Multiple R-squared:  0.3208,   Adjusted R-squared:  0.2282 

# p-value dla wartoœci statystyki F: 0.006857 -> ma³e, czyli w danych jest
# w ogóle jakaœ zale¿noœæ -> model ma jakiœ sens

# b) czy lepszy jest model bez State i Female?

l2 <- lm(Sales~.-State-Female-HS, data=ci)
summary(l2) # Multiple R-squared:  0.3202,   Adjusted R-squared:  0.2611 

anova(l,l2)
# p-value: 0.9789 - du¿e, czyli przyjmujemy hipotezê, czyli mniejszy model 
# jest lepszy, czyli te dwie zmienne mog¹ byæ usuniête z modelu

# c) porównaæ R^2

# w pierwszym modelu modyfikowane R^2 znacz¹co mniejsze 

