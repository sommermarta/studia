# ostatecznie

ostat <- read.csv("dane\\cup98test_noclass.txt")
ostat2 <- cbind(ostat[,1:470], cos1=NA, cos2=NA, ostat[471:ncol(ostat)])

modyfikacja5 <- function(dane){
  dane <- dane[,-c(2,4,232,233,234,236,237,241,242,244,245,246,247,248:252)]
  dane
}

ostat3 <- modyfikacja(ostat2)
ostat4 <- modyfikacja3(ostat3)
ostat5 <- modyfikacja2(ostat4)
ostat5 <- modyfikacja5(ostat5)


# hist_lub_bar <- function(i){
#   if(is.numeric(ostat5[,i])){
#     hist(ostat5[,i])
#   } else barplot(table(ostat5[,i]))  
# }
# 
# num_char_fac <- function(i){
#   if(is.numeric(ostat5[,i])){
#     "numeric"
#   } else if(is.character(ostat5[,i])){
#     "character"
#   } else "factor"
# }
# 
# i <- 18
# hist_lub_bar(i)
# names(ostat5)[i]
# num_char_fac(i)
# head(ostat5[,i])
# table(ostat5[,i])
# sum(is.na(ostat5[,i]))
# mean(ostat5[,i], na.rm=TRUE)
# 
# levostat <- numeric(ncol(ostat5))
# for(i in 1:ncol(ostat5)){
#   levostat[i] <- length(levels(ostat5[,i]))
# }
# 
# levtest <- numeric(ncol(ostat5))
# for(i in 1:ncol(ostat5)){
#   levtest[i] <- length(levels(testowy3[,i]))
# }
# 
# names(testowy3)[which(levostat!=levtest)]




# testowy3$RFA_3
# ostat5$RFA_3

bayes_pred <- predict(bayes, 
                      newdata=ostat5,
                      type="class")

zzz <- which(bayes_pred==1)

# g <- file("komu_wyslac_ostat.txt", "w")
# writeLines(as.character(zzz), g)
# close(g)
# 
# 
# predict(drzewo, newdata=ostat4, type="class")
# 
# dim(ostat5)
# length(levels(testowy3[,3]))
