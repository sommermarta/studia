
source("kody//pakiety_i_funkcje.R")
source("kody//czyszczenie_danych.R")

###############################################################
######################## NAIWNY BAYES #########################
###############################################################

# w Bayesie wszysktie zmienne musza byc faktorami, wiec faktoryzuje:

modyfikacja2 <- function(dane){
  for(i in 1:ncol(dane)){
    if(is.numeric(dane[,i]) & names(dane)[i]!="TARGET_D"){
      dane[,i] <- discretize(dane[,i], method="interval", categories = 2, 
                             labels = c("A", "B"),     
                             onlycuts=FALSE)
    }
  }
dane
}

# nowy zbior treningowy i testowy:

treningowy3 <- modyfikacja5(modyfikacja2(treningowy2))
testowy3 <- modyfikacja5(modyfikacja2(testowy2))

# dopasowuje model:

bayes <- naiveBayes(TARGET_B~.-TARGET_D, data=treningowy3, laplace=0.2)

# wyznaczam klasy i pstwa na zbiorze testowym:

bayes_pred <- predict(bayes, newdata = testowy3, type="class")
bayes_prawd <- predict(bayes, newdata=testowy3, type="raw")[,2]

# zobaczmy wyniki:

tab <- tabela(bayes_pred, testowy3$TARGET_B)

procent(tab)
czulosc(tab)
precyzja(tab)

roc(bayes_prawd, testowy3$TARGET_B)
auc(bayes_prawd, testowy3$TARGET_B)

zarobek(testowy3, bayes_pred)
zarobek_gdy_do_wszystkich(testowy3)

