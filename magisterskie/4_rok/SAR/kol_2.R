
a <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                butter_r.txt",header=TRUE)
head(a)
attach(a)

# 1

komb <- Breed:Age

# wykres srednich
sr <- tapply(Butterfat_r,komb,mean)
stripchart(Butterfat_r~komb,pch=1,vertical=TRUE)
lines(1:10,sr,pch=20,type="b",cex=2)

# wykres kwantylowy
par(mfrow=c(2,2))
tapply(Butterfat_r,komb,qqnorm)  # wygladaja na w miare normalny :D

# wykres skrzynkowy
par(mfrow=c(1,1))
plot(komb,Butterfat_r)

# test shapiro-wilka:
naz <- unique(Age)
naz2 <- unique(Breed)
sh <- matrix(0,nrow=2,ncol=5)

for(i in 1:2){
   for(j in 1:5){
      sh[i,j] <- shapiro.test(a$Butterfat_r[a$Age==naz[i] & 
                                               a$Breed==naz2[j]])$p.value
   }
}

sum(sh <=0.05)  # wszystkie maja wiec rozklad normalny

# test bartletta:
bartlett.test(Butterfat_r~komb)  # p-value = 0.5848, wiec przyjmujemy hipoteze,
                                 # o rownosci wariancji
# test levene'a:
g <- lm(Butterfat_r~komb)
rez <- lm(abs(g$residuals)~komb)
summary(rez)   # 0.481 -> duze, wiec test levene'a tez potwierdza rownosc wariancji

# wszystkie zalozenia anovy spelnione

# 2

par(mfrow=c(1,2))
interaction.plot(Breed,Age,Butterfat_r)  # ciezko stwierdzic
interaction.plot(Age,Breed,Butterfat_r)

l <- lm(Butterfat_r~Breed*Age)
summary(l)

anova(l)  # 0.5807 -> duze, to nie ma interakcji

l2 <- lm(Butterfat_r~Breed+Age)
anova(l2,l)  # nie ma interakcji

# 3

anova(l2)  # Age nie jest istotne, ale Breed tak

# 4

summary(l2) # 0.2499 - 0.02059 - 0.00528

# 5

l4 <- aov(Butterfat_r~Breed)
TukeyHSD(l4)
# Jersey-Guernsey -> tylko tu nie ma roznic

##########################################################################


b <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/SAR/DANE/
                earthquake.txt",header=TRUE)
head(b)
attach(b)

bb <- scale(b[,2:3])

pc <- princomp(~.,cor=FALSE,data=as.data.frame(bb))
which(pc$scores[,1]==max(pc$scores[,1]))  # 3

pc$loadings
# pierwsza skladowa glowna = 0.707*body + 0.707*surface = 0.707(body + surface)

summary(pc) # tu jest odchyl. stand., a nie wariancja, ale to juz mozna sobie podniesc do kwadratu i tyle

#                          Comp.1    Comp.2
# Standard deviation     1.075718 0.8796965











