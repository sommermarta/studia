################# zadanie 2 #############################

# wczytanie danych

dead <- c(15,17,22,38,144)
razem <- c(297,242,312,299,285)
dawka <- c(0.0,62.5,125.0,250.0,500.0)
szkod <- data.frame(dead,razem,dawka)
head(szkod)

# a) model logitowy

model <- glm(cbind(dead, razem-dead)~dawka, family="binomial", data=szkod)
summary(model)

# b)

# z testu Walda -> zmienna dawka jest istotna

1 - pchisq(model$deviance, model$df.residual)   
# nie mozemy odrzucic hipotezy o tym, ze model jest dobrze dopasowany

# c)

100*(1 - model$deviance/model$null.deviance)  
# ladnie -> 97%

################# zadanie 3 #############################

# wczytanie danych

rok <- c(1987,1988,1989)
kwartal1 <- c(217,156,149)
kwartal2 <- c(144,163,137)
kwartal3 <- c(177,205,189)
kwartal4 <- c(131,130,117)
wypadki <- data.frame(rok,kwartal1,kwartal2,kwartal3,kwartal4)
head(wypadki)

# a)

rok <- c(rep(1987,4),rep(1988,4),rep(1989,4))
osoby <- c(217,144,177,131,156,163,205,130,149,137,189,117)
kwartal <- rep(c(1,2,3,4),3)
nowe <- data.frame(rok,kwartal,osoby)
head(nowe)

model <- glm(osoby~rok+kwartal, data=nowe, family="poisson")
summary(model)
# obie zmienne sa istotne

1 - pchisq(model$deviance, model$df.residual) 
# ale model jest zle dopasowany

# wszystkie zmienne istotne, ale model zle dopasowany, wiec moze byc 
# nadwyzka rozproszenia

# b)

phihat <- sum(residuals(model, type="pearson")^2)/model$df.residual
phihat
summary(model, dispersion=phihat)
# zmienne juz nie sa istotne...

























