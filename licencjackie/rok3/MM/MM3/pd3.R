#pd3

par(mfrow=c(1,1))
sigma <- c(0.5,1,2,3)
N <- seq(20,300,10); N
a <- matrix(numeric(length(sigma)*length(N)), nrow=length(N)); a
?matrix
L <- 50
beta <- c(0.5,2,1,0.5,0.3)
blad <- numeric(L)

for(j in 1:length(sigma)){      # pêtla po sigmach 
  for(n in N){                
    for(i in 1:L){
      x1 <- rnorm(n)
      x2 <- rnorm(n)
      x3 <- rnorm(n)
      x4 <- rnorm(n)
      
      eps <- rnorm(n,0,sigma[j])
      
      y <- 0.5 + 2*x1 + x2 + 0.5*x3 + 0.3*x4 + eps
      D <- data.frame(y,x1,x2,x3,x4)
      mod <- lm(y~.,data=D)
      wsp <- as.numeric(mod$coef)
      blad[i] <- sum((wsp-beta)^2)
    }
    a[n/10-1,j] <- mean(blad)
  }
}
a

matplot(N, a, type="l")