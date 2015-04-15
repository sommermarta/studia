library("tm")
library("Matrix") # do tworzenia macierzy rzadkich  --->  Matrix()  
library("glmnet")
# install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at", 
#                  type="source")
library("tm.corpus.Reuters21578")
library("ROCR")

##################### Funkcje pomocnicze ###################################

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
roc <- function(pred_prawdopod, prawdziwe_klasy,...){
  pred <- prediction(pred_prawdopod, prawdziwe_klasy)
  perf <- performance(pred, measure="tpr",x.measure="fpr")
  plot(perf,col="red",...)
  abline(0,1)
  lines(c(0.5,0.5),c(-0.1,1.1),lty=2,col="green")
}
auc <- function(pred_prawdopod, prawdziwe_klasy){
  pred <- prediction(pred_prawdopod, prawdziwe_klasy)
  performance(pred, "auc")@y.values[[1]]
}

##########################################################################

data(Reuters21578)
Reuters21578[[1]]
meta(Reuters21578[[1]])

train_test <- unlist(meta(Reuters21578, tag="lewissplit"))
train_ind <- train_test == "TRAIN"
test_ind <- train_test == "TEST"
class <- unlist(lapply(meta(Reuters21578, tag="topics_cat"), function(x) {is.element("crude", x)})) * 1
class_train <- class[train_ind]
class_test <- class[test_ind]
reuters <- Reuters21578 # retain original file

ile_tren <- 1000
ile_test <- 1000

tren <- reuters[train_ind]
tren <- tren[1:ile_tren]
tren_klasy <- class_train[1:ile_tren]

test <- reuters[test_ind]
test <- test[1:ile_test]
test_klasy <- class_test[1:ile_test]

macierz <- DocumentTermMatrix(c(tren,test),
                   control = list(removePunctuation = TRUE,
                                  stopwords = TRUE))

macierz_tren <- macierz[1:ile_tren,]
macierz_test <- macierz[(ile_tren+1):(ile_tren+ile_test),]

m_tren <- as.matrix(macierz_tren)
m_test <- as.matrix(macierz_test)

ile_chce_wektorw_w_rozkladzie <- 100

rozklad_svd <- svd(m_tren, nu=ile_chce_wektorw_w_rozkladzie, nv=ile_chce_wektorw_w_rozkladzie)
m_tren2 <- rozklad_svd$u
m_test2 <- m_test %*% rozklad_svd$v %*% diag(1/rozklad_svd$d[1:ile_chce_wektorw_w_rozkladzie])

model <- cv.glmnet(m_tren2, tren_klasy, family="binomial")
pred_klasy <- predict(model, newx=m_test2, type="class")
pred_prob <- predict(model, newx=m_test2, type="response")

roc(pred_prob, test_klasy)
auc(pred_prob, test_klasy)

# ktore_slowa_maja_wplyw <- attributes(coef(model))$Dimnames[[1]][which(coef(model)>0)]
# ktore_slowa_maja_wplyw

