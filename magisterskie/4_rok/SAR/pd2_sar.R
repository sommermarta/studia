
# praca domowa nr 2 - zad. 6.2 - SAR - Marta Sommer - BSMAD - 237503

eksperyment <- function(n){
   
   beta <- as.matrix(c(1,1,1,0,0,0,0,0,0))
   L <- 50 
   
   aa <- 0
   bb <- 0
   
   for(i in 1:L){
      
      eps <- rnorm(n)
      X <- matrix(0,ncol=9,nrow=n)
      
      for(i in 1:n){
         X[i,] <- rnorm(9)
      }
      
      Y <- X %*% beta + eps
      
      dim(Y) <- c(n,1)
      XX <- data.frame(X)
      
      l <- lm(Y~.-1,data=XX)
      
      a <- step(l,direction="backward",k=2,trace=0)
      b <- step(l,direction="backward",k=log(n),trace=0)
      
      if(setequal(names(a$coefficients),c("X1","X2","X3"))) aa <- aa + 1
      if(setequal(names(b$coefficients),c("X1","X2","X3"))) bb <- bb + 1
      
   }
   
   pstwo_a <- aa/L
   pstwo_b <- bb/L
   
   c(pstwo_a,pstwo_b)
   
}

ek <- function(){
   c(eksperyment(25),eksperyment(50),eksperyment(75),eksperyment(100),
     eksperyment(125),eksperyment(150),eksperyment(175),eksperyment(200))
}

wykres <- function(){
   mac <- matrix(ek(),ncol=2,byrow=TRUE)
   nn <- c(25,50,75,100,125,150,175,200)
   
   matplot(nn,mac,type=c("b","b"),pch=20,col=c("red","blue"),ylim=c(0,1),
           ylab="pstwo",xlab="n")
   
   nazwy <- c("AIC","BIC")
   legend("topleft",nazwy,fill=c("red","blue"))
}

wykres()
