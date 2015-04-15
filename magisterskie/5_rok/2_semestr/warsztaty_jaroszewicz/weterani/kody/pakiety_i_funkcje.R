###############################################################
######################### PAKIETY #############################
###############################################################

library("arules")
library("ROCR")
library("e1071")
library("glmnet")
library("rpart")
library("corrplot")
library("corrgram")
library("psych")
library("MASS")
library("stringi")

###############################################################
################### PRZYDATNE FUNKCJE #########################
###############################################################

tabela <- function(predykcja_klasy, klasy_prawdziwe){
  table(predykcja_klasy, klasy_prawdziwe)
}

procent <- function(t){
  100*sum(diag(t))/sum(t)
}

czulosc <- function(t){
  if(sum(t[2,])==0) return(0) else t[2,2]/(sum(t[2,]))
}

precyzja <- function(t){
  if(sum(t[,2])==0) return(0) else t[2,2]/sum(t[,2])
}

auc <- function(pred_prawdopod, prawdziwe_klasy){
  pred <- prediction(pred_prawdopod, prawdziwe_klasy)
  performance(pred, "auc")@y.values[[1]] 
}

roc <- function(pred_prawdopod, prawdziwe_klasy, ...){
  pred <- prediction(pred_prawdopod, prawdziwe_klasy)
  perf <- performance(pred, measure="tpr", x.measure="fpr")
  plot(perf, col="red", ...)
  abline(0,1)
  lines(c(0.5,0.5), c(-0.1,1.1),lty=2, col="green")
}

roc2 <- function(pred_prawdopod, prawdziwe_klasy, ...){
  pred <- prediction(pred_prawdopod, prawdziwe_klasy)
  perf <- performance(pred, measure="tpr", x.measure="fpr")
  plot(perf, col="red", add=TRUE, ...)
}

zarobek <- function(zbior_testowy, predykcja_klasy){
  dobre_jedynki <- which(predykcja_klasy==1 & zbior_testowy$TARGET_B==1)
  wplat <- sum(zbior_testowy$TARGET_D[dobre_jedynki])
  wplat - 0.5*length(which(predykcja_klasy==1))
}

zarobek_gdy_do_wszystkich <- function(zbior_testowy){
  sum(zbior_testowy$TARGET_D[which(zbior_testowy$TARGET_B==1)])-0.5*nrow(zbior_testowy)
}
