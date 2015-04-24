dane <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\magisterskie\\rok5\\4 semestr\\seminarium\\ordinal_regression\\carpref.txt",
                   header = TRUE)
dane

# sex: 0 - kobiety, 1 - mezczyzni
# response: dostepnosc klimatyzacji i ukladu wspomagania kierownicy w samochodzie jest:
#   1 - niewazne / malo wazne
#   2 - wazne
#   3 - bardzo wazne

#   1 < 2 < 3


# MODEL WIELOMIANOWY (ZMIENNA NOMINALNA, A NIE PORZADKOWA)

mac <- matrix(dane$frequency, ncol=3, byrow=TRUE)
wiek <- rep(unique(dane$age),2)
plec <- rep(c(0,1),each=3)

nowedane <- data.frame(plec, wiek, mac)
colnames(nowedane)[3:5] <- c("ocena1", "ocena2", "ocena3")
nowedane

library("nnet")

model1 <- multinom(cbind(ocena1, ocena2, ocena3) ~ plec + wiek, data = nowedane)
summary(model1)

# Pstwo tego, ze taka kobieta uzna dostepnosc klimatyzacji i wspomagania
# za niewazny/malo wazny czynnik?

predict(model1, data.frame(plec=0, wiek="18-23"), type="probs")

# MODEL PROPORCJONALNYCH HAZARDOW

library("MASS")

dane

dane$response <- as.factor(dane$response)
dane

model2 <- polr(response ~ age + sex, weights = frequency, data = dane)
summary(model2)

predict(model2, data.frame(sex=0, age="18-23"), type="probs")

































