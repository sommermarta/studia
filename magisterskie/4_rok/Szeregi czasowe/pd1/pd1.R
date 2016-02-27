library("quantmod")

# 1.4

# a)

zt <- rnorm(501)
xt <- numeric(500)

for(i in 1:500){
   xt[i] <- zt[i+1]-0.8*zt[i]
}

t <- ts(xt)
plot(t)

# b)

acf(t)

# c)

lag1 <- Lag(xt)
lag2 <- Lag(xt,k=2)

l1 <- lm(xt~lag1)
l2 <- lm(xt~lag1+lag2)

summary(l1)
summary(l2)

# 1.5

# a)

x0 <- 1
eps <- rnorm(500,0,0.1)
xt <- numeric(500)
xt[1] <- x0
for(i in 2:500){
   xt[i] <- xt[i-1]*0.8 + eps[i]
}

# b)

plot(xt,type="l")
pacf(xt)
acf(xt)

# c)

sr <- mean(xt)
gamma0 <- (sum((xt-sr)^2))/500
gamma1 <- (sum((xt[1:499]-sr)*(xt[2:500]-sr)))/500
gamma2 <- (sum((xt[1:498]-sr)*(xt[3:500]-sr)))/500
gamma3 <- (sum((xt[1:497]-sr)*(xt[4:500]-sr)))/500

ro0 <- 1
ro1 <- gamma1/gamma0
ro2 <- gamma2/gamma0
ro3 <- gamma3/gamma0

ro1/ro0
ro2/ro1
ro3/ro2

# d)

lag1 <- Lag(xt)
l <- lm(xt~lag1)
summary(l)

# e)

x0 <- 1
eps <- rnorm(500,0,0.1)
xt <- numeric(500)
xt[1] <- x0
for(i in 2:500){
   xt[i] <- xt[i-1] + eps[i]
}

plot(xt,type="l")
pacf(xt)
acf(xt)


















