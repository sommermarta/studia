############### zad.1 ##########################

library("forward")
data(chapman)
head(chapman)

model <- glm(y~., data=chapman, family="binomial")
summary(model)

1 - pchisq(model$deviance, model$df.residual)
# wyglada na dobrze dopasowany

predict(model, data.frame(age=60,
                          highbp=140,
                          lowbp=90,
                          chol=200,
                          height=69,
                          weight=200), type="response")  # 22%

############### zad.2 ##########################

birth <- read.table("C:\\Users\\Marta\\Dropbox\\GLM\\GLM\\zbiory_danych\\Birth.txt", 
                    header=TRUE)
head(birth)

# a)

model <- glm(children~age+0, data=birth, family="poisson")
summary(model)
# zmienna age istotna

1 - pchisq(model$deviance, model$df.residual)  
# zle dopasowany

# log(mi) = 0.013*age, czyli:
# mi = exp(0.013*age)

# b)

model2 <- glm(children~age+0, data=birth, 
              family="poisson"(link="identity"))
summary(model2)
# zmienna age istotna

1 - pchisq(model2$deviance, model2$df.residual) 
# nadal zle dopasowanie

# mi = 0.019*age

# c)

# kanoniczna:
exp(0.013)  # liczba dzieci zwieksza sie tyle razy

# identycznosciowa:
# srednia liczba dzieci zwieksza sie o b, czyli 0.019

# d)

plot(birth$age, predict(model, birth, type="response"))
lines(birth$age, predict(model2, birth, type="response"), col="blue")















