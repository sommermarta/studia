# Matdal "Ekonometria"
# Green "Econometrics"

# 1. Gajda
# 2. Matdal
# 3. Green

# zad.1

a <- read.table("H:\\Windows7\\Desktop\\z1.csv",sep=";",header=TRUE)
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
elas_cena
elas_dochod <- l$coefficients[3]*c2/p2
elas_dochod


































































































