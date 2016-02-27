library("systemfit")
library("tseries")

# 1

w <- read.csv2("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Ekonometria\\spr4\\AIDS.csv")
head(w)

w <- cbind(w[,1:3],WHOUSE=w[,5],WNDO=w[,4],w[,6:8],PHOUSE=w[,10],
           PNDO=w[,9],LREXP=w[,11])
head(w)

food <- w$WFOOD ~ log(w$PFOOD)+log(w$PCLOTH)+log(w$PSO)+log(w$PHOUSE)+
   log(w$PNDO)+log(w$LREXP)
cloth <- w$WCLOTH ~ log(w$PFOOD)+log(w$PCLOTH)+log(w$PSO)+log(w$PHOUSE)+
   log(w$PNDO)+log(w$LREXP)
so <- w$WSO ~ log(w$PFOOD)+log(w$PCLOTH)+log(w$PSO)+log(w$PHOUSE)+
   log(w$PNDO)+log(w$LREXP)
house <- w$WHOUSE ~ log(w$PFOOD)+log(w$PCLOTH)+log(w$PSO)+log(w$PHOUSE)+
   log(w$PNDO)+log(w$LREXP)

lista <- as.list(c(food,cloth,so,house))
lista
names(lista) <- c("food","cloth","so","house")

model <- systemfit(lista,method="SUR")

# 2

co <- model$coefficients
coef <- matrix(co,ncol=7,nrow=4,byrow=TRUE)
rownames(coef) <- c("food","cloth","so","house")
colnames(coef) <- c("alfa","gamma_pfood","gamma_pcloth","gamma_pso",
                    "gamma_phouse","gamma_pndo","beta")
coef

ndo <- numeric(7)
suma <- apply(coef,2,sum)
ndo[1] <- 1-suma[1]
ndo[7] <- 0-suma[7]
ndo[2:6] <- 0-suma[2:6]
ndo
coef <- rbind(coef,ndo=ndo)
coef

m <- matrix(0,nrow=10,ncol=28)
m[1,3] <- 1
m[1,9] <- -1
m[2,4] <- 1
m[2,16] <- -1
m[3,5] <- 1
m[3,23] <- -1
m[4,11] <- 1
m[4,17] <- -1
m[5,12] <- 1
m[5,24] <- -1
m[6,19] <- 1
m[6,25] <- -1
m[7,2:6] <- 1
m[8,9:13] <- 1
m[9,16:20] <- 1
m[10,23:27] <- 1

d <- numeric(10)
linearHypothesis(model,m,d)   # hipoteza niestety nie jest spelniona

# 3

wsr <- apply(w[,1:5],2,mean)
wsr

# elastycznosc dochodowa:

ed <- 1+coef[,7]/wsr
ed

# elastycznosc cenowa:

ec <- matrix(0,ncol=5,nrow=5)
for(i in 1:5){
   for(j in 1:5){
      delta <- ifelse(i==j,1,0)
      ec[i,j] <- -delta+coef[i,j+1]/wsr[i]-coef[i,7]/wsr[i]*wsr[j]
   }
}
ec
rownames(ec) <- c("food","cloth","so","house","ndo")
colnames(ec) <- c("pfood","pcloth","pso","phouse","pndo")
ec   # popyt na i-te dobro ze wzgledu na wzrost ceny dobra j-tego

# 4

adf.test(w$WFOOD,k=0)
adf.test(w$WCLOTH,k=0)
adf.test(w$WSO,k=0)
adf.test(w$WHOUSE,k=0)   # TYLKO TO WYSZLO STACJONARNE (trzeba przymknac na to oko)
adf.test(w$WNDO,k=0)

adf.test(w$PFOOD,k=0)
adf.test(w$PCLOTH,k=0)
adf.test(w$PSO,k=0)
adf.test(w$PHOUSE,k=0)  
adf.test(w$PNDO,k=0)
adf.test(w$LREXP,k=0)  # z testu Dickey'a-Fullera wynika, ze wszystkie 
                       # szeregi oprocz whouse s¹ niestacjonarne

# sprawdzmy zatem, czy sa I(1):

adf.test(diff(w$WFOOD),k=0)
adf.test(diff(w$WCLOTH),k=0)
adf.test(diff(w$WSO),k=0)
adf.test(diff(w$WHOUSE),k=0)
adf.test(diff(w$WNDO),k=0)   

adf.test(diff(w$PFOOD),k=0)
adf.test(diff(w$PCLOTH),k=0)
adf.test(diff(w$PSO),k=0)       
adf.test(diff(w$PHOUSE),k=0)
adf.test(diff(w$PNDO),k=0)   # wszystkie wyszly stacjonarne, czyli szeregi
                             # sa I(1)

re <- residuals(model)
re

adf.test(re[,1],k=0) 
adf.test(re[,2],k=0)
adf.test(re[,3],k=0)
adf.test(re[,4],k=0)   # jedno tylko wychodzi niestacjonarne :D

# czyli miedzy szeregami wystepuje kointegracja 

# 6

lista2 <- as.list(c(diff(w$WFOOD) ~ re[-length(re[,1]),1]+diff(log(w$PFOOD))+
                       diff(log(w$PCLOTH))+diff(log(w$PSO))+diff(log(w$PHOUSE))+
                       diff(log(w$PNDO))+diff(log(w$LREXP)),
                   diff(w$WCLOTH) ~ re[-length(re[,1]),2]+diff(log(w$PFOOD))+
                      diff(log(w$PCLOTH))+diff(log(w$PSO))+diff(log(w$PHOUSE))+
                      diff(log(w$PNDO))+diff(log(w$LREXP)),
                   diff(w$WSO) ~ re[-length(re[,1]),3]+diff(log(w$PFOOD))+
                      diff(log(w$PCLOTH))+diff(log(w$PSO))+diff(log(w$PHOUSE))+
                      diff(log(w$PNDO))+diff(log(w$LREXP)),
                   diff(w$WHOUSE) ~ re[-length(re[,1]),4]+diff(log(w$PFOOD))+
                      diff(log(w$PCLOTH))+diff(log(w$PSO))+diff(log(w$PHOUSE))+
                      diff(log(w$PNDO))+diff(log(w$LREXP))))
lista2

names(lista2) <- c("food","cloth","so","house")
lista2

model2 <- systemfit(lista2,method="SUR")

co2 <- model2$coefficients
coef2 <- matrix(co2, ncol=8,nrow=4,byrow=TRUE)
rownames(coef2) <- c("food","cloth","so","house")
colnames(coef2) <- c("intercept","alfa","gamma_pfood","gamma_pcloth","gamma_pso",
                    "gamma_phouse","gamma_pndo","beta")
coef2

# alfy wyszly ujemne -> odchylenie od stanu rownowagi mniejsze niz zero

m2 <- matrix(0,nrow=10,ncol=32)
m2[1,4] <- 1
m2[1,11] <- -1
m2[2,5] <- 1
m2[2,19] <- -1
m2[3,6] <- 1
m2[3,27] <- -1
m2[4,13] <- 1
m2[4,20] <- -1
m2[5,14] <- 1
m2[5,28] <- -1
m2[6,22] <- 1
m2[6,29] <- -1
m2[7,3:7] <- 1
m2[8,11:15] <- 1
m2[9,19:23] <- 1
m2[10,27:31] <- 1

d2 <- numeric(10)
linearHypothesis(model2,m2,d2)   # hipoteza niestety nie jest spelniona


wsr <- apply(w[,1:5],2,mean)
wsr

# elastycznosc dochodowa:

beta <- c(coef2[,8],ndo=-sum(coef2[,8]))
beta

ed <- 1+beta/wsr
ed

# elastycznosc cenowa:

gamma <- coef2[,3:7]
suma <- apply(coef2,2,sum)
gamma_ndo <- 0-suma[3:7]
gamma <- rbind(gamma,ndo=gamma_ndo)
gamma

ec <- matrix(0,ncol=5,nrow=5)
for(i in 1:5){
   for(j in 1:5){
      delta <- ifelse(i==j,1,0)
      ec[i,j] <- -delta+gamma[i,j]/wsr[i]-beta[i]/wsr[i]*wsr[j]
   }
}
ec
rownames(ec) <- c("food","cloth","so","house","ndo")
colnames(ec) <- c("pfood","pcloth","pso","phouse","pndo")
ec   # popyt na i-te dobro ze wzgledu na wzrost ceny dobra j-tego

