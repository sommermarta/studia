# zad. 1

library("faraway")
data(sat)
head(sat)
attach(sat)

# 1

l <- lm(total~expend+ratio+salary+takers,data=sat)
summary(l)
summary(l)$coefficients[,1]

# (Intercept)      expend       ratio      salary      takers 
# 1045.971536    4.462594   -3.624232    1.637917   -2.904481 

summary(l)$r.squared  # [1] 0.8245623

# 2

summary(l)

# za istotne mozna uznac takers i Intercept, bo p-value ich t-testu jest 
# male

# 3

# mala wartosc p-testu widoczna w summary() swiadczy o tym, ze mozemy mowic
# o jakiejs zaleznosci zmiennych, ze ktoras zmienna jest zalezna, ze model
# w ogole ma sens (odrzucamy hipoteze testu F)

# 4

# dla salary:

# wykres czesciowej regresji

ly <- lm(total~expend+ratio+takers,data=sat)$residuals
lx <- lm(salary~expend+ratio+takers,data=sat)$residuals

ll <- lm(ly~lx)
plot(lx,ly)
abline(ll,col="red")

ll$coefficients[2] # wiekszy od 0, choc minimalnie -> niemniej jednak 
                   # zmienna jest istotna

# wykres czesciowych rezyduow

summary(l)
prplot(l,3) # zmienna istotna

# dla takers:

ly <- lm(total~expend+ratio+salary,data=sat)$residuals
lx <- lm(takers~expend+ratio+salary,data=sat)$residuals

ll <- lm(ly~lx)
plot(lx,ly)
abline(ll,col="red")

ll$coefficients[2] # zmienna jest istotna

# wykres czesciowych rezyduow

summary(l)
prplot(l,4) # zmienna istotna

# 5

# przedzial ufnosci dla wspolczynnika przy zmiennej expend

alfa <- 0.05
beta <- l$coefficients[2]
se <- summary(l)$coefficients[2,2]
q <- qt(1-alfa/2,nrow(sat)-5)

l <- beta - q*se
p <- beta + q*se

l;p

# 6

l <- lm(total~expend+ratio+salary+takers,data=sat)
a <- l$coefficients

a[1] + a[2]*6 + a[3]*16 + a[4]*34 + a[5]*35  # 968.7918

# 7

# H0: expend + salary
# H1: expend+ratio+salary+takers

l <- lm(total~expend+ratio+salary+takers,data=sat)
l2 <- lm(total~expend+salary,data=sat)

anova(l,l2)

# p-value male, wiec odrzucamy hipoteze, wiec wiekszy model jest lepszy

# 8

n <- 50
e <- numeric(50)
sat
y <- sat$total
x <- sat[,1:4]

for(i in 1:n){
   
   xx <- as.matrix(x[-i,])
   yy <- y[-i]
   
   l <- lm(yy~xx)
   
   xt <- as.matrix(x[i,])
   yt <- y[i]
   
   c <- l$coefficients
   beta <- as.numeric(c)
   e[i] <- mean(( yt - cbind(1,xt)%*%beta )^2)
}

e
mean(e)

