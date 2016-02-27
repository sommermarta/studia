### PRACA DOMOWA DATA MINING 2 --- MARTA SOMMER BSMAD 237503 ###

library("microbenchmark")

metoda_najw_wiar_logist <- function(y,x,iteracje,beta_0,war_stop){
   
   beta_new <- beta_0
   
   for(i in 1:iteracje){
      
      beta <- as.numeric(beta_new)
      p <- numeric(nrow(x))
      for(j in 1:nrow(x)){
         p[j] <- exp(beta%*%t(t((x[j,]))))/
            (1+exp(t(beta)%*%t(t((x[j,])))))
      }
      w <- diag(p*(1-p))
      
      z <- x%*%beta+solve(w)%*%(y-p)
      
      beta_new <- lm(z~x-1,weights=diag(w))$coefficients
      if(all(abs(beta-beta_new) < war_stop)) return(beta_new)
      beta_old <- beta_new
   }
   beta_new
   
}


# przyklad 1

h <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/SAheart.data",sep=",",header=TRUE)
head(h)
attach(h)

x <- h[,-c(1,11)]
head(x)

xx <- as.matrix(x)
head(xx)

for(i in 1:nrow(xx)){
   if(xx[i,5]=="Present") xx[i,5] <- 1 else xx[i,5] <- 0
}
head(xx)

xxx <-  cbind(rep(1,nrow(xx)),
              matrix(as.numeric(xx),nrow=nrow(xx),ncol=ncol(xx)))
head(xxx)

y <- h$chd
y

l <- glm(chd~.-row.names,data=h,family="binomial")  
l$coefficients

metoda_najw_wiar_logist(y,xxx,10,numeric(ncol(xxx)),0.000000001)

microbenchmark(glm(chd~.-row.names,data=h,family="binomial")$coefficients,
               metoda_najw_wiar_logist(y,xxx,10,numeric(ncol(xxx)),
                                       0.000000001))

# estymatory dokladnie takie same, jak w funkcji wbudowanej glm()
# widac, ze niestety moja metoda duzo wolniejsza

# przyklad 2

u <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/urine.txt",
                sep=" ",header=TRUE)
head(u)
attach(u)

l <- glm(presence~.,data=u,family="binomial")  
l$coefficients

x <- u[,-1]
head(x)

xxx <-  cbind(rep(1,nrow(x)),
              matrix(as.numeric(as.matrix((x))),nrow=nrow(x),ncol=ncol(x)))
head(xxx)

y <- as.character(u$presence)
y[which(u$presence=="yes")] <- 1
y[which(u$presence=="no")] <- 0
y <- as.numeric(y)
y

l <- glm(presence~.,data=u,family="binomial")  
l$coefficients

metoda_najw_wiar_logist(y,xxx,100,numeric(ncol(xxx)),0.0000001)

microbenchmark(glm(presence~.,data=u,family="binomial")$coefficients,
               metoda_najw_wiar_logist(y,xxx,100,numeric(ncol(xxx)),
                                       0.0000001))

# estymatory nieznacznie sie roznia (na jakichs dalekich miejscach 
#                                    po przecinku)
# widac, ze moja funkcja jakies 10 razy wolniejsza
