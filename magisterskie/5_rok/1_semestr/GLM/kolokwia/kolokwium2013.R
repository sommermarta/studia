############# zad.1 ######################

urine <- read.table("C:\\Users\\Marta\\Dropbox\\GLM\\GLM\\zbiory_danych\\urine.txt", 
                    header=TRUE)
head(urine)

# a)

model <- glm(presence ~ ., data=urine, family="binomial")
summary(model)
# tak, istotne jest urea, clacium i prawie mmho

# b)

modelb <- step(model, direction="backward")
summary(modelb)   
# calcium, urea, mmho, sg

modelnull <- glm(presence ~ 1, data=urine, family="binomial")
modelf <- step(modelnull, direction="forward")
summary(modelf)
# tylko intercept...

# c)

1 - pchisq(model$deviance, model$df.residual)
1 - pchisq(modelb$deviance, modelb$df.residual)
1 - pchisq(modelf$deviance, modelf$df.residual)  # ten jest zle dopasowany

100*(1 - model$deviance/model$null.deviance)
100*(1 - modelb$deviance/modelb$null.deviance)  # ma troche gorsze r^2 niz model pelny
100*(1 - modelf$deviance/modelf$null.deviance)

anova(modelb, model, test="Chisq")  # lepszy mniejszy

############# zad.2 ######################

rok <- 1981:1993
zachorowania <- c(12,14,33,50,67,74,123,141,165,204,253,246,240)

dane <- data.frame(rok, zachorowania)
dane

# a)

model <- glm(zachorowania~rok, data=dane, family="poisson")
summary(model)

# b)

1 - pchisq(model$deviance, model$df.residual)   
# model jest zle dopasowany

halfnorm(rstudent(model))
plot(residuals(model, "deviance"))
# ewidentnie nie sa bialym szumem...

# c)

model2 <- glm(zachorowania~rok+I(rok^2), data=dane, family="poisson")
summary(model2)

1 - pchisq(model2$deviance, model$df.residual) 
# ten jest juz dobrze dopasowany :D
# intercept jest istotny

model0 <- glm(zachorowania~rok+I(rok^2)+0, data=dane, family="poisson")
summary(model0)
anova(model0, model2, test="Chisq")
# wiekszy model lepszy, ok :D
