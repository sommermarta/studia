library("bstats")
library("lmtest")
library("FinTS")

# model jednoczynnikowy:

p1 <- read.csv2("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Ekonometria\\spr3\\portfele_z1.csv",sep=";",header=TRUE)
head(p1)

wig1 <- p1[,2]

l1 <- vector("list",16)
for(i in 1:16){
   x <- p1[,i+5]
   l1[[i]] <- lm(x~wig1)
}
l1

w1 <- numeric(16)
bg <- numeric(16)
gq <- numeric(16)
dw <- numeric(16)
for(i in 1:16){
   w1[i] <- ifelse(white.test(l1[[i]])$p.value<0.05,1,0)
   bg[i] <- ifelse(bptest(l1[[i]])$p.value<0.05,1,0)
   gq[i] <- ifelse(gqtest(l1[[i]], fraction=0.33, order.by=~wig1)$p.value<0.05,1,0)
   dw[i] <- ifelse(dwtest(l1[[i]],alternative="two.sided")$p.value<0.05,1,0)
}

w1 # heteroskedastycznosc
bg # heteroskedastycznosc
gq # heteroskedastycznosc
dw # autokorelacja


rm1 <- numeric(0)
for(i in 1:16){
   rm1[i] <- l1[[i]]$coefficients[2]
}

# tabele dla RM:

rm1
tabela1_rm <- matrix(rm1,nrow=4)
tabela1_rm

cat("\\begin{tabular}[c|c|c|c]")
for(i in 1:4){
   cat("\n & ")
   for(j in 1:4){
      cat("$",tabela1_rm[i,j],"$")
      if(j!=4) cat(" & ") else cat(" \\\\ \\cline{5-6} ")
   }
}
cat("\\end{tabular}")

t <- nrow(p1)
n <- 16
k <- 1

alfa1 <- numeric(16)
for(i in 1:16){
   alfa1[i] <- l1[[i]]$coefficients[1]
}
alfa1

m <- matrix(0,nrow=97,ncol=16)
for(i in 1:16){
   a <- l1[[i]]$residuals
   for(j in 1:97){
      m[j,i] <- a[j]
   }
}
m
dim(m)

sigma <- (t(m)%*%m)/t
sigma

head(p1)
v <- var(p1$WIG)

mi <- mean(p1$WIG)

grs <- (t/n)*((t-n-k)/(t-k-1))*((alfa1%*%solve(sigma)%*%t(t(alfa1)))/(1+mi%*%solve(v)%*%t(t(mi))))
grs
1-pf(grs,n,t-n-k)   # ma³e -> odrzucamy hipoteze, czyli alfa nie sa zerami


# model trójczynnikowy:

l2 <- vector("list",16)
for(i in 1:16){
   x <- p1[,i+5]
   l2[[i]] <- lm(x~wig1+p1$SMB+p1$HML)
}

w1 <- numeric(16)
bg <- numeric(16)
gq <- numeric(16)
dw <- numeric(16)
for(i in 1:16){
   w1[i] <- ifelse(white.test(l2[[i]])$p.value<0.05,1,0)
   bg[i] <- ifelse(bptest(l2[[i]])$p.value<0.05,1,0)
   gq[i] <- ifelse(gqtest(l2[[i]], fraction=0.33, order.by=~wig1)$p.value<0.05,1,0)
   dw[i] <- ifelse(dwtest(l2[[i]],alternative="two.sided")$p.value<0.05,1,0)
}
w1
bg
gq
dw

rm2 <- numeric(0)
smb2 <- numeric(0)
hml2 <- numeric(0)

for(i in 1:16){
   rm2[i] <- l2[[i]]$coefficients[2]
   smb2[i] <- l2[[i]]$coefficients[3]
   hml2[i] <- l2[[i]]$coefficients[4]
}

tabela2_rm <- matrix(rm2,nrow=4)
tabela2_smb <- matrix(smb2,nrow=4)
tabela2_hml <- matrix(hml2,nrow=4)

tabela2_rm 
tabela2_smb 
tabela2_hml


t <- nrow(p1)
n <- 16
k <- 3

alfa2 <- numeric(16)
for(i in 1:16){
   alfa2[i] <- l2[[i]]$coefficients[1]
}
alfa2

m <- matrix(0,nrow=97,ncol=16)
for(i in 1:16){
   a <- l2[[i]]$residuals
   for(j in 1:97){
      m[j,i] <- a[j]
   }
}
sigma2 <- (t(m)%*%m)/t

h <- matrix(c(p1$WIG,p1$SMB,p1$HML),nrow=97)

v <- cov(h)
mi <- apply(h,2,mean)
mi
grs <- (t/n)*((t-n-k)/(t-k-1))*((alfa2%*%solve(sigma2)%*%t(t(alfa2)))/
                                   (1+mi%*%solve(v)%*%t(t(mi))))
grs
1-pf(grs,n,t-n-k)


# dla p2

l22 <- vector("list",16)
for(i in 1:16){
   x <- p2[,i+5]
   l22[[i]] <- lm(x~wig1+p2$SMB+p2$HML)
}

w1 <- numeric(16)
bg <- numeric(16)
gq <- numeric(16)
dw <- numeric(16)
for(i in 1:16){
   w1[i] <- ifelse(white.test(l22[[i]])$p.value<0.05,1,0)
   bg[i] <- ifelse(bptest(l22[[i]])$p.value<0.05,1,0)
   gq[i] <- ifelse(gqtest(l22[[i]], fraction=0.33, order.by=~wig1)$p.value<0.05,1,0)
   dw[i] <- ifelse(dwtest(l22[[i]],alternative="two.sided")$p.value<0.05,1,0)
}
w1
bg
gq
dw

rm2 <- numeric(0)
smb2 <- numeric(0)
hml2 <- numeric(0)

for(i in 1:16){
   rm2[i] <- l22[[i]]$coefficients[2]
   smb2[i] <- l22[[i]]$coefficients[3]
   hml2[i] <- l22[[i]]$coefficients[4]
}

tabela2_rm <- t(matrix(rm2,nrow=4))
tabela2_smb <- t(matrix(smb2,nrow=4))
tabela2_hml <- t(matrix(hml2,nrow=4))

tabela2_rm 
tabela2_smb 
tabela2_hml


t <- nrow(p2)
n <- 16
k <- 3

alfa2 <- numeric(16)
for(i in 1:16){
   alfa2[i] <- l22[[i]]$coefficients[1]
}
alfa2

m <- matrix(0,nrow=97,ncol=16)
for(i in 1:16){
   a <- l22[[i]]$residuals
   for(j in 1:97){
      m[j,i] <- a[j]
   }
}
sigma2 <- (t(m)%*%m)/t

h <- matrix(c(p1$WIG,p1$SMB,p1$HML),nrow=97)

v <- cov(h)
mi <- apply(h,2,mean)
mi
grs <- (t/n)*((t-n-k)/(t-k-1))*((alfa2%*%solve(sigma2)%*%t(t(alfa2)))/
                                   (1+mi%*%solve(v)%*%t(t(mi))))
grs
1-pf(grs,n,t-n-k)   # odrzucamy hipotezê

###########################################################################
# model czteroczynnikowy:

l3 <- vector("list",16)
for(i in 1:16){
   x <- p1[,i+5]
   l3[[i]] <- lm(x~wig1+p1$SMB+p1$HML+p1$WML)
}

l3

w1 <- numeric(16)
bg <- numeric(16)
gq <- numeric(16)
dw <- numeric(16)
for(i in 1:16){
   w1[i] <- ifelse(white.test(l3[[i]])$p.value<0.05,1,0)
   bg[i] <- ifelse(bptest(l3[[i]])$p.value<0.05,1,0)
   gq[i] <- ifelse(gqtest(l3[[i]], fraction=0.33, order.by=~wig1)$p.value<0.05,1,0)
   dw[i] <- ifelse(dwtest(l3[[i]],alternative="two.sided")$p.value<0.05,1,0)
}

w1 # heteroskedastycznosc
bg # heteroskedastycznosc
gq # heteroskedastycznosc
dw # autokorelacja


rm3 <- numeric(0)
smb3 <- numeric(0)
hml3 <- numeric(0)
wml3 <- numeric(0)

for(i in 1:16){
   rm3[i] <- l3[[i]]$coefficients[2]
   smb3[i] <- l3[[i]]$coefficients[3]
   hml3[i] <- l3[[i]]$coefficients[4]
   wml3[i] <- l3[[i]]$coefficients[5]
}


tabela3_rm <- matrix(rm3,nrow=4)
tabela3_smb <- matrix(smb3,nrow=4)
tabela3_hml <- matrix(hml3,nrow=4)
tabela3_wml <- matrix(wml3,nrow=4)


tabela3_rm
tabela3_smb
tabela3_hml
tabela3_wml

t <- nrow(p1)
n <- 16
k <- 4

alfa3 <- numeric(16)
for(i in 1:16){
   alfa3[i] <- l3[[i]]$coefficients[1]
}
alfa3

m <- matrix(0,nrow=97,ncol=16)
for(i in 1:16){
   a <- l3[[i]]$residuals
   for(j in 1:97){
      m[j,i] <- a[j]
   }
}
sigma3 <- (t(m)%*%m)/t

h <- matrix(c(p1$WIG,p1$SMB,p1$HML,p1$WML),nrow=97)

v <- cov(h)
mi <- apply(h,2,mean)
mi
grs <- (t/n)*((t-n-k)/(t-k-1))*((alfa3%*%solve(sigma3)%*%t(t(alfa3)))/(1+mi%*%solve(v)%*%t(t(mi))))
grs
1-pf(grs,n,t-n-k)

# dla p2:

l33 <- vector("list",16)
for(i in 1:16){
   x <- p2[,i+5]
   l33[[i]] <- lm(x~p2$WIG+p2$SMB+p2$HML+p2$WML)
}

t <- nrow(p2)
n <- 16
k <- 4

alfa3 <- numeric(16)
for(i in 1:16){
   alfa3[i] <- l33[[i]]$coefficients[1]
}
alfa3

m <- matrix(0,nrow=97,ncol=16)
for(i in 1:16){
   a <- l33[[i]]$residuals
   for(j in 1:97){
      m[j,i] <- a[j]
   }
}
sigma3 <- (t(m)%*%m)/t

h <- matrix(c(p2$WIG,p2$SMB,p2$HML,p2$WML),nrow=97)

v <- cov(h)
mi <- apply(h,2,mean)
mi
grs <- (t/n)*((t-n-k)/(t-k-1))*((alfa3%*%solve(sigma3)%*%t(t(alfa3)))/(1+mi%*%solve(v)%*%t(t(mi))))
grs
1-pf(grs,n,t-n-k)

# 6
# dwoch pierwszych nie ma sensu - tylko trzeci

# pierwsza metoda: (wazona metoda najmniejszych kwadratow)

l3

xx <- matrix(0,nrow=16,ncol=5)
xx[,1] <- 1
for(i in 1:16){
   xx[i,2:5] <- l3[[i]]$coefficients[2:5]
}

xx
sigma3

p1

r_sr <- apply(p1[6:21],2,mean) 
r_sr

gamma <- solve(t(xx)%*%solve(sigma3)%*%xx)%*%t(xx)%*%solve(sigma3)%*%r_sr
gamma  # to jest premia za ryzyko -> srednio 0.2 procenta

lm(r_sr~xx+0 ,weights=diag(sigma3))

va <- (1/97)*( solve( t(xx)%*%solve(sigma3)%*%xx ) + rbind(0,cbind(0,v)) )
va

gamma[1]/sqrt(va[1,1])
1-pt(gamma[1]/sqrt(va[1,1]),97-5)  # gammma 0 wyszlo rowne zero
qt(0.95,97-5)

gamma[2]/sqrt(va[2,2])
1-pt(gamma[2]/sqrt(va[2,2]),97-5)

gamma[3]/sqrt(va[3,3])
1-pt(gamma[3]/sqrt(va[3,3]),97-5)

gamma[4]/sqrt(va[4,4])
1-pt(gamma[4]/sqrt(va[4,4]),97-5)

gamma[5]/sqrt(va[5,5])
1-pt(gamma[5]/sqrt(va[5,5]),97-5)

# jesli hml zwiekszy sie o jeden procent, to ma to wplyw na wartosc naszego portfele, jesli sie zwieksza pozostale to juz raczej nie

# druga metoda:

eps <- numeric(5*16)
mac <- matrix(0,nrow=80,ncol=97)

for(j in 1:97){
   
   for(i in 1:16){
      eps[i] <- l3[[i]]$residuals[j]
   }
   
   eps[17:32] <- eps[1:16]*p1[j,2]
   eps[33:48] <- eps[1:16]*p1[j,3]
   eps[49:64] <- eps[1:16]*p1[j,4]
   eps[65:80] <- eps[1:16]*p1[j,5]
   
   mac[,j] <- eps
}

mac

gt <- apply(mac,1,sum)
gt