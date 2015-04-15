library("rjags")
library("coda")
library("mcmcplots")

setwd("C:\\Users\\sommerm\\Dropbox\\uczenie_maszynowe\\jags")

# ZADANIE NR 1

# a) zapisalismy tamto do pliku

# b)

N <- 1000
x <- rnorm(N, 0, 5) 

jags <- jags.model("example1.bug", data = list("x" = x, "N" = N),
                   n.chains = 1, n.adapt = 100) 

# N - liczba obserwacji
# x - wartosci wylowowane
# n.chains - liczba lancuchow, ktore chcemy wylosowac
# n.adapt - ile probek poczatkowych odrzucamy na poczatku

update(jags, n.adapt = 1000) 

# probkowanie Gibbsa:
s <- coda.samples(jags, c('mu', 'sigma'), 1000, thin=1)

# 1000 - ile tego losujemy
# thin - co ktora probke wybieramy (thin==1 -> wszystkie)

summary(s)
plot(s)
autocorr.plot(s)   # autokorelacja
g <- geweke.diag(s)
# 10% pierwszych obserwacji porownuje z 50% ostatnich obserwacji
# i liczy roznice srednich i wariancji

# liczymy p-value, zeby miec formalny test:
2*(1-pnorm(abs(g[[1]]$z), 0, 1)) # no i nie mozemy odrzucic hipotezy, czyli roznic nie ma

mcmcplot(s, dir = getwd())
denplot(s)
caterplot(s)

# ZADANIE NR 2

# y_i = x_i*beta + alfa +epsilon_i
# beta = 2
# alfa = 3
# x_i ~ U(0, 50)
# epsilon_i ~ N(0, 4)
# epsilon[i] ~ dnorm(0, tau)
# tau ~ dgamma(0001, 0.001)

n <- 500
x <- runif(n, 0, 1)
eps <- rnorm(n, 0, 2)
beta <- 2
alfa <- 3

y <- beta*x + alfa + eps

# teraz mamy tylko danego x i y i chcemy wyestymowac bete i alfe!

jags2 <- jags.model("example2.bug", 
                    data = list("x" = x, "n" = n, "y" = y),
                    n.chains = 1, n.adapt = 100) 

update(jags, n.adapt = 1000) 

s <- coda.samples(jags2, c('alfa', 'beta'), 10000, thin=1)
summary(s)
plot(s)

# ZADANIE NR 3

n <- 100
beta0 <- -5
beta1 <- 0.01
x <- 1:n

mi <- beta0 + beta1*x
p <- 1/(1 + exp(-mi))
 
y <- numeric(n)
for(i in 1:n){
  y[i] <- rbinom(1, 1, p[i])
}

jags3 <- jags.model("example3.bug", 
                    data = list("n" = n, "y" = y, "x" = x),
                    n.chains = 1, n.adapt = 1000) 

s <- coda.samples(jags3, c('beta0', 'beta1'), 10000, thin=1)
summary(s)
plot(s)


