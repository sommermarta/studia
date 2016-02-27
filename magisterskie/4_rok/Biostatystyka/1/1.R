library("survival")
library("foreign")

# zad.1

sz <- matrix(c(4,5,8,10,17,17,17,19,24,34,34,
               7,8,8,8,8,8,8,11,11,12,12,15,16,18,21,23,24,25,27,
               1,1,1,1,0,0,0,1,0,0,0,
               1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
               rep(1,11),
               rep(2,19),
               2,3,1,3,2,1,1,1,1,1,2,
               1,1,2,3,1,2,2,3,2,1,2,2,1,1,1,2,1,3,1),
             nrow=30)
sz <- as.data.frame(sz)
names(sz) <- c("czas","zdarzenie","rodzaj.terapii","wiek")

sz.KM <- survfit(Surv(czas,zdarzenie) ~ rodzaj.terapii, data=sz, 
                 conf.type="none")

plot(sz.KM,col=c("red","blue"), lty=1:2, main="Krzywe przezycia",
     xlab="miesiace",ylab="p-stwo przezycia")
legend(0.4, 0.27, c("szczepionka 1","szczepionka 2"),col=c("red","blue"), 
       lty=1:2,cex=0.7)

summary(sz.KM)               

sz.test <- survdiff(Surv(czas,zdarzenie) ~ rodzaj.terapii, data=sz)
sz.test

sz.test$exp
sz.test$var

stat_prost <- sum((sz.test$obs-sz.test$exp)^2/sz.test$exp)
p.val <- 1-pchisq(stat_prost,1)

# zad.2

sz.strata <- survdiff(Surv(czas,zdarzenie) ~ rodzaj.terapii + strata(wiek),
                      data=sz)
sz.strata

sz.strata$exp

e <- rowSums(sz.strata$exp)
o <- rowSums(sz.strata$obs)
stat_prost <- sum((e-o)^2/e)
p.val <- 1-pchisq(stat_prost,1)

# zad.3

a <- read.csv2("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\1\\cyfra_short.csv",
               sep=",")
head(a)

a.KM_level <- survfit(Surv(survtime,survind)~level,data=a,conf.type="none")

plot(a.KM_level, col=c("red","blue"), lty=1:2, main="Krzywe przezycia",
     xlab="miesiace",ylab="p-stwo przezycia")
legend(0.4, 0.27, c("niski poziom markera","wysoki poziom markera"),
       col=c("red","blue"), lty=1:2,cex=0.7)

a.KM_stage <- survfit(Surv(survtime,survind)~stage,data=a,conf.type="none")

plot(a.KM_stage, col=c("red","blue","green"), lty=1:3, main="Krzywe przezycia",
     xlab="miesiace",ylab="p-stwo przezycia")
legend(0.4, 0.35, c("TNM 1","TNM 2","TNM 3"),
       col=c("red","blue","green"), lty=1:3,cex=0.7)

a.KM_histpat <- survfit(Surv(survtime,survind)~histpat,data=a,conf.type="none")

plot(a.KM_histpat, col=c("red","blue","green"), lty=1:3, main="Krzywe przezycia",
     xlab="miesiace",ylab="p-stwo przezycia")
legend(0.4, 0.35, c("gruczolakorak","wielokomorkowiec","plaskonablonkowiec"),
       col=c("red","blue","green"), lty=1:3,cex=0.7)


a.KM <- survdiff(Surv(survtime,survind)~level+strata(stage),data=a)
a.KM

tt <- a.KM$exp
rownames(tt) <- c("niski poziom markera","wysoki poziom markera")
colnames(tt) <- c("TNM 1","TNM 2","TNM 3")

