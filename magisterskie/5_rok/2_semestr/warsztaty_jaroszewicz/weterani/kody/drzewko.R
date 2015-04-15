
source("kody//pakiety_i_funkcje.R")
source("kody//czyszczenie_danych.R")

###############################################################
############################ DRZEWA ###########################
###############################################################

drzewo <- rpart(TARGET_B~.-TARGET_D, data=treningowy2, cp=0.0001, minsplit=5,
                weights=ifelse(TARGET_D > 0, 1, 0.5))
plot(drzewo)
text(drzewo)

drzewo_pred <- predict(drzewo, newdata=testowy2, type="class")
drzewo_prawd <- predict(drzewo, newdata=testowy2, type="prob")[,2]

tab <- tabela(drzewo_pred, testowy2$TARGET_B)

procent(tab)
czulosc(tab)
precyzja(tab)

roc(drzewo_prawd, testowy2$TARGET_B)
auc(drzewo_prawd, testowy2$TARGET_B)

zarobek(testowy2, drzewo_pred)
zarobek_gdy_do_wszystkich(testowy2)

?rpart
# 
# svm <- svm(TARGET_B~.-TARGET_D, data=treningowy2, type="C", kernel="radial",
#            probability=TRUE)
# 
# svm_pred <- predict(svm, newdata=testowy2, type="class")
# svm_prawd <- predict(drzewo, newdata=testowy2, type="prob")[,2]
# 
# roc(svm_prawd, testowy2$TARGET_B)
# auc(svm_prawd, testowy2$TARGET_B)
# 
# sum(as.numeric(as.character(svm_pred)))








