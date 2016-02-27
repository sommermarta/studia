#zad1

cred <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/MM/DANE/kredit.asc", header=T)
head(cred)

#a

m <- glm(kredit~., data=cred, family="binomial")
summary(m)

#b

m$coef

#c

x <- c(1,cred[1,-1]); x     #pomijamy zmienn¹ objaœniaj¹c¹ (czyli zmienn¹ kredit)
x <- as.numeric(x); x
b <- as.numeric(m$coef); b
il <- b%*%x
il
p <- 1/(1+exp(-il))   #pstwo sukcesu (udzielenie sukcesu)     TAKI WZÓR!!!!!!!!!
p        #czyli mamy pstwo, ¿e pierwsza osoba dostanie kredyt

predict(m, newdata=cred[1,-1],type="response")      # to samo pstwo co wy¿ej, tylko wyliczone za pomoc¹ specjalnej funkcji R-a - "response", ¿eby nam zwróci³a szukane pstwo

#d
#wybór zmiennych - kryterium bayesa

?step
n<-dim(cred)[1]   #liczba wierszy
mm <- step(m,direction="backward",k=log(n))

#e


#czlow <- c(1,4,5,4,3,33)
#cz <- data.frame(t(czlow))   #bo tu musi byæ poziome, a nie pionowe, czy coœ w tym stylu

cz <- data.frame(1,4,5,4,3,33)
names(cz) <- c("famges","rate","sparkont","moral","laufkont","laufzeit")
cz

predict(mm, newdata=cz, type="response")

#f
?anova
anova(m,mm,test="Chisq")   #14  - liczba stopni swobody
                           # H0: bety wiêksze s¹ równe zero
                           #odrzucamy H0 tzn, ¿e wiêkszy model jest lepszy (patrzymy na p-value)


#zad2

#pd3

N <- seq(50,300,10); N
L <- 50
beta <- c(0.5,1,1)
blad <- numeric(L)
a <- numeric(length(N))

j <- 1
y<-numeric(L)

for(n in N){
        
    for(i in 1:L){
      
      x1 <- rnorm(n)
      x2 <- rnorm(n)
      
      for(k in 1:n){
        
        p<- 1/(1+exp(-(0.5+x1[k]+x2[k]))) 
        y[k] <- rbinom(n,prob=p,size=1)
      }
     
      D <- data.frame(y,x1,x2)
      mod <- glm(y~.,data=D)
      wsp <- as.numeric(mod$coef)
      blad[i] <- sum((wsp-beta)^2)
    }
    
    a[j] <- mean(blad)
    j <- j+1
}

a


plot(N, a, type="l") 

























