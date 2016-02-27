# Matdal "Ekonometria"
# Green "Econometrics"

# 1. Gajda
# 2. Matdal
# 3. Green

# zad.1

a <- read.table("H:\\Windows7\\Desktop\\Ekonometria\\z1.csv",sep=";",header=TRUE)
a


c2 <- a[,2]/a[,5]
p2 <- a[,3]/(a[,6]*a[,5])

y <- p2
x <- matrix(c(c2,a[,4]/a[,5]),ncol=2)
colnames(x) <- c("cena2","dochod2")
y
x

l <- lm(y~x)
l$coefficients
summary(l)

lp <- lm(log(y)~log(x))
summary(lp)

# nic nie jest istotne

elas_cena <- l$coefficients[2]*c2/p2
mean(elas_cena)
elas_dochod <- l$coefficients[3]*c2/p2
mean(elas_dochod)

# zad2

zywn <- read.table("H:\\Windows7\\Desktop\\Ekonometria\\zad2.csv",sep=";",header=TRUE)
zywn
attach(zywn)

l <- lm(log(q)~log(p)+log(doch),data=zywn)
summary(l)
l$coefficients

# (Intercept)      log(p)   log(doch) 
# 4.0472535  -0.1188943   0.2411538 

ll <- lm(log(q)~log(p)*log(doch),data=zywn)
summary(ll)
ll$coefficients

# (Intercept)           log(p)        log(doch) log(p):log(doch) 
# 8.0289771       -0.9962511       -0.7183930        0.2111850 

e1cen <- l$coefficients[2]
e1doch <- l$coefficients[3]
e1cen; e1doch
# log(p) -0.1188943 
# log(doch) 0.2411538 

e2cen <- ll$coefficients[2]+ll$coefficients[4]*log(zywn$doch)
mean(e2cen) #[1] -0.09084108
e2doch <- ll$coefficients[3]+ll$coefficients[4]*log(zywn$p)
mean(e2doch) #[1] [1] 0.239355

#stabilnosc

install.packages("strucchange")
library("strucchange")

sctest(log(q)~log(p)+log(doch),data=zywn,type="Chow",point=15) #popyt nie jest stabilny, czyli nie mo¿na traktowaæ tych danych tak samo

# d

l2 <- lm(log(q)~log(p)+log(doch),data=zywn[1:15,])
summary(l2)
l2$coefficients

l3 <- lm(log(q)~log(p)+log(doch),data=zywn[16:30,])
summary(l3)
l$coefficients


# zad3

gas <- read.csv2("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Ekonometria\\lab1_zad3.csv",
                 sep=";",header=TRUE)
attach(gas)

l <- lm(log(G/Pop)~log(Y)+log(Pg)+log(Pnc)+log(Puc),data=gas)
summary(l)

elas_cen_now <- l$coefficients[4]
elas_cen_uz <- l$coefficients[5]
elas_cen_doch <- l$coefficients[2]


# zad4

# elas.cen.now = 1

el_nowe <- l$coefficients[4]
names(summary(l))
names(l)
tabela <- summary(l)$coefficients

(tabela[4,1]-0)/tabela[4,2]
2*min(pt(-1.459463,36-5),pt(-1.459463,36-5,lower.tail=FALSE))
2*min(pt(-0.998450,36-5),pt(-0.998450,36-5,lower.tail=FALSE))
2*min(pt(18.160007,36-5),pt(18.160007,36-5,lower.tail=FALSE))
tabela

t.value <- (tabela[2,1]-1)/tabela[2,2]
p.value <- pt(t.value,36-5,lower.tail=FALSE)

library("strucchange")
sctest(log(G/Pop)~log(Y)+log(Pg)+log(Pnc)+log(Puc),data=gas,type="Chow")
sctest(log(G/Pop)~log(Y)+log(Pg)+log(Pnc)+log(Puc),data=gas,type="Chow",point=14)


plot(l)

plot(G/Pop~Year)


?plot
































