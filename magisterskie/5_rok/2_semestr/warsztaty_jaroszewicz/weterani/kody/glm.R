
source("kody//pakiety_i_funkcje.R")
source("kody//czyszczenie_danych.R")

###############################################################
######################## GLMNET - LOGIT #######################
###############################################################

model <- glm(TARGET_B~.-TARGET_D, data=treningowy2, family=binomial(link = "logit"))
pred_klasy <- predict(model, newx=m_test2, type="class")
pred_prob <- predict(model, newx=m_test2, type="response")


as.matrix(data.frame(a=c(2,3,3), b=c(4,4,4)))


sum(is.na(treningowy3))


?cv.glmnet

?glm

???????????????????????????????????????????






