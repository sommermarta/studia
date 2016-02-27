# kartka 4    

# zad 1  - rozk³ad normalny o znanym sigma i nieznamym mi
# H: mi = mi0
# K: mi > mi0

x <- c(1.36,1.38,1.14,1.37,1.27,1.30,1.15,1.21,1.20,1.33,
       1.29,1.28,1.27,1.32,1.18,1.29,1.23,1.33,1.36,1.25)
sr <- mean(x); sr
mi0 <- 1.20
sigma <- 0.07
n <- 20
alfa <- 0.04

test <- (sr-mi0)*sqrt(n)/sigma; test     # statystyka testowa
q <- qnorm(1-alfa)
test > q     # wysz³o TRUE, wiêc odrzucamy hipotezê zerow¹
             # czyli wytrzyma³oœæ przewy¿sza 1.20


#zad 2

# 1. sposób
x <- c(142,151,148,151,145,150,141)
sr <- mean(x); sr
mi0 <- 150                 #rozk³ad normalny
a <- 0.05
n <- length(x)
s <- sd(x)
te <- (sr-mi0)*sqrt(n)/s; te
q <- qt(1-a/2,n-1)
te > q  | te < (-q)   # F, czyli hipoteza zerowa jest prawdziwa 
                      # - waga wskazuje wskazuje poprawn¹ wartoœæ

# 2. sposób

t.test(x,alternative="two.sided",mu=150)            # two.sided, bo mamy w K nierównoœæ
t.test(x,alternative="two.sided",mu=150)$p.value    # p przekracza poziom istotnoœci testu, 
                                                    # wiêc nie mamy podstaw do odrzucenia hipotezy
                                                    # bo 0.09>0.05

# zad3

# a

x <- c(2852,3060,2631,2819,2805,2835,2955,2595,2690,2723,2815,2914)
sr <- mean(x); sr
s <- sd(x)
n <- length(x); n
a <- 0.03

# rozk³ad jest dowolny, ale mamy ma³¹ próbê, wiêc zak³adamy normalnoœæ

#dla œredniej

q <- qt(1-a/2,n-1)
sr+c(-1,1)*q*s/sqrt(n)

#dla odchylenia

qs <- qchisq(1-a/2,n-1)
qss <- qchisq(a/2,n-1)
sqrt(s^2*(n-1)/qs); sqrt(s^2*(n-1)/qss)

# b

mi0 <- 2900
te=(sr-mi0)*sqrt(n)/s; te
q <- qt(1-a,n-1); q
te < (-q)                        #  T - odrzucamy hipotezê
t.test(x, alternative="less", mu=2900)$p.value   # 0.02 < a - odrzucamy


# zad4

x1 <- c(145,150,153,148,141,152,146,154,139,148)
x2 <- c(152,150,147,155,140,146,158,152,151,143,153)

sr1 <- mean(x1)
sr2 <- mean(x2)
n1 <- length(x1)
n2 <- length(x2)

# H: mi1 = mi2
# K: mi1 < mi2

s1 <- sd(x1)
s2 <- sd(x2)

N <- (n1+n2)/(n1*n2)
L <- (n1-1)*s1^2+(n2-1)*s2^2
M <- n1+n2-2
tes <- (sr1-sr2)/sqrt((L/M)*N); tes
a <- 0.05
q <- qt(1-a,M)
tes < (-q)      # F -> hipoteza zerowa jest prawdziwa

t.test(x1,x2,alternative="less",var.equal=T)$p.value   # przyjmujemy hipotezê, bo 0.17 > a


# zad8

n <- 200
k <- 14
a <- 0.05
p <- k/n
p0 <- 0.04

tes <- (p-p0)/sqrt(p0*(1-p0)/n); tes
q <- qnorm(1-a)
tes > q                   # T, czyli nale¿y poprawiæ produkcjê, odrzucamy hipotezê zerow¹

prop.test(14,200,p=0.04,alternative="greater")$p.value
