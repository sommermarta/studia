###############################################################
######################## WCZYTUJE DANE ########################
###############################################################

weterani <- read.csv("dane\\cup98train.txt")
weterani$"TARGET_B" <- as.factor(weterani$"TARGET_B")

# head(weterani)
# dim(weterani)

###############################################################
########### PODZIAL NA ZBIOR TRENINGOWY I TESTOWY #############
###############################################################

a <- nrow(weterani)*c(0.5, 0.5)

# 7356
set.seed(645111)

ind_tren <- sample(nrow(weterani), a[1])
roznica <- setdiff(1:nrow(weterani), ind_tren)
ind_test <- sample(roznica, a[2])

treningowy <- weterani[ind_tren, ]
testowy <- weterani[ind_test, ]

###############################################################
################## ZMINENNE OBJASNIANE ########################
###############################################################

# head(treningowy$TARGET_B, 100)
# head(treningowy$TARGET_D, 100)
# 
# # wykresy:
# 
# barplot(table(treningowy$TARGET_B))
# barplot(table(treningowy$TARGET_D))
# barplot(table(treningowy$TARGET_D)[-1])


###############################################################
################# CZYSZCZENIE DANYCH ##########################
###############################################################

# hist_lub_bar <- function(i){
#   if(is.numeric(treningowy[,i])){
#     hist(treningowy[,i])
#   } else barplot(table(treningowy[,i]))  
# }
# 
# num_char_fac <- function(i){
#   if(is.numeric(treningowy[,i])){
#     "numeric"
#   } else if(is.character(treningowy[,i])){
#     "character"
#   } else "factor"
# }
# 
# i <- 43
# names(treningowy)[i]
# num_char_fac(i)
# head(treningowy[,i])
# hist_lub_bar(i)
# table(treningowy[,i])
# sum(is.na(treningowy[,i]))
# mean(treningowy[,i], na.rm=TRUE)

modyfikacja <- function(dane){
  
  dane[,3] <- ifelse(dane[,3] %in% c(006,010,010002,010010,
                                     011,011002,014,014002,015,
                                     015002,018,018002,
                                     021002,027,029,056,072002,
                                     089,0090),"1","0")
  dane[,14] <- ifelse(dane[,14]!="XXXX", "1", "0")
#   dane[,15][which(dane[,15]==" ")] <- NA
#   dane[,18][which(dane[,18]==" ")] <- NA
#   dane[,19][which(dane[,19]==" ")] <- NA
  dane[,25][is.na(dane[,25])] <- 3.861312
  dane[which(dane[,26] %in% c(" ","U","J")),26] <- " "
  dane[,17][is.na(dane[,17])] <- 61.46857
  dane[,43][is.na(dane[,43])] <- 2.494782
  dane[,16][is.na(dane[,16])] <- 27.82367
  dane[,27][is.na(dane[,27])] <- 5.384038
  dane[,54][is.na(dane[,54])] <- 5.020638
  dane[,467][is.na(dane[,467])] <- 0
  dane[,480][is.na(dane[,480])] <- 31.40661
  
  dane <- dane[,-c(5, 7, 8, 9, 12, 20:24, 29:42, 44, 51, 52, 55,
                   74, 196:198, 362:384, 413:456, 468,
                   470, 474, 477:479, 481)]
  dane$"TCODE" <- as.factor(dane$"TCODE")
  dane$"MDMAUD" <- as.factor(dane$"MDMAUD")
  
  dane
}

# nowy, oczyszczony zbior treningowy i testowy:

treningowy2 <- modyfikacja(treningowy)
testowy2 <- modyfikacja(testowy)

# badanie korelacji:

# czynum <- numeric(ncol(treningowy2))
# for(i in 1:ncol(treningowy2)){
#   czynum[i] <- is.numeric(treningowy2[,i])
# }
# ktore_numeryczne <- which(czynum==1)
# 
# M <- cor(treningowy2[,ktore_numeryczne])
#cor.plot(M[1:10, 1:10], numbers=TRUE)

# pocz <- seq(1, dim(M)[1], 10)
# kon <- c(seq(10, dim(M)[1], 10), dim(M)[1])
# poczkon <- data.frame(pocz, kon)
# 
# g <- file("wykresy_korelacji.txt", "w")
# for(i in 1:nrow(poczkon)){ 
#   for(j in 1:nrow(poczkon)){
#     co <- stri_paste("cor.plot(M[",poczkon[i,1],":",poczkon[i,2],",",
#           poczkon[j,1],":",poczkon[j,2],"], numbers=TRUE, main=\"M[",poczkon[i,1],
#           ":",poczkon[i,2],",",poczkon[j,1],":",poczkon[j,2],"]\")",collapse="")
#     writeLines(co, g)
#   }
# }
# close(g)
# 
# pdf("wykresy_korelacji.pdf")
# for(i in 1:nrow(poczkon)){ 
#   for(j in 1:nrow(poczkon)){
#     cor.plot(M[poczkon[i,1]:poczkon[i,2],
#                poczkon[j,1]:poczkon[j,2]],
#              numbers=TRUE, main=stri_paste("M[",poczkon[i,1],":",poczkon[i,2],
#                                            ",",poczkon[j,1],":",poczkon[j,2],"]",
#                                            collapse=""))
#   }
# }
# dev.off()
# 
# nazwy <- colnames(M)
# wymiar <- dim(M)[1]
# 
# kol <- ifelse((which(M>0.8 | M<(-0.8)) %% wymiar)==0, 
#               trunc(which(M>0.8 | M<(-0.8))/wymiar),
#               trunc(which(M>0.8 | M<(-0.8))/wymiar)+1)
# wier <- which(M>0.8 | M<(-0.8)) %% wymiar
# if(any(wier==0)) wier[wier==0] <- wymiar
# 
# skorel <- matrix(0, nrow=length(kol), ncol=2)
# for(i in 1:length(kol)){
#   skorel[i,1] <- nazwy[kol[i]]
#   skorel[i,2] <- nazwy[wier[i]]
# }
# skorel
# 
# takie_same <- numeric(nrow(skorel))
# for(i in 1:nrow(skorel)){
#   if(skorel[i,1]==skorel[i,2]){
#     takie_same[i] <- 1
#   }
# }
# takie_same
# 
# skorel <- skorel[-which(takie_same==1), ]
# skorel
# 
# gdzie <- numeric(nrow(skorel))
# for(i in 1:nrow(skorel)){
#   por <- skorel[i,]
#   for(j in i:nrow(skorel)){
#     if(all(c(skorel[j,2], skorel[j,1])==por)){
#       gdzie[i] <- j      
#     }
#   }
# }
# gdzie
# 
# duble <- unique(sort(gdzie))[-1]
# skorel <- skorel[-duble,]
# do_usuniecia <- unique(skorel[,2])
# do_usuniecia
# 
# modyfikacja3 <- function(dane){
#   dane[,-which(names(treningowy2) %in% do_usuniecia)]
# }
# treningowy3 <- modyfikacja3(treningowy2)
# 
# g <- file("wykresy_korelacji.txt", "w")
# writeLines("-c(",g)
# ktory <- which(names(treningowy2) %in% do_usuniecia)
# for(i in 1:(length(ktory)-1)){
#   writeLines(stri_paste("-",ktory[i],",",collapse=""), g)
# }
# writeLines(stri_paste("-",ktory[length(ktory)],collapse=""), g)
# writeLines(")",g)
# 
# close(g)


modyfikacja3 <- function(dane){
  dane <- dane[,-c(
    48,
    49,
    67,
    72,
    73,
    74,
    75,
    76,
    86,
    88,
    90,
    93,
    94,
    95,
    98,
    99,
    100,
    101,
    104,
    106,
    107,
    109,
    112,
    113,
    118,
    120,
    122,
    126,
    127,
    128,
    129,
    130,
    131,
    134,
    136,
    139,
    140,
    141,
    142,
    143,
    144,
    145,
    146,
    147,
    148,
    149,
    152,
    153,
    154,
    157,
    160,
    161,
    163,
    164,
    165,
    166,
    168,
    169,
    170,
    171,
    177,
    180,
    181,
    182,
    183,
    184,
    185,
    186,
    187,
    188,
    189,
    190,
    193,
    195,
    200,
    209,
    213,
    214,
    215,
    216,
    235,
    251,
    252,
    253,
    265,
    269,
    272,
    273,
    275,
    299,
    300,
    303,
    304,
    309,
    310,
    311,
    312,
    322,
    324,
    325,
    353,
    355,
    360,
    365,
    367,
    369
  )
  ]
  dane
}

treningowy2 <- modyfikacja3(treningowy2)
testowy2 <- modyfikacja3(testowy2)
