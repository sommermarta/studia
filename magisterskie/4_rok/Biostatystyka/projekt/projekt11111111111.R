library("foreign")
library("survival")

################### przygotowanie danych ################################

b <- read.dta("BreastFeeding.dta")
head(b)
n <- nrow(b)

rasa_biala <- ifelse(b$race==1,1,0)
rasa_czarna <- ifelse(b$race==2,1,0)

bb <- cbind(b[,-3],rasa_biala,rasa_czarna)
head(bb)
names(bb)[2] <- "stopp"
head(bb,2)

#################### kaplan-meier ##########################################

km <- survfit(Surv(feed,stopp) ~ 1, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

km <- survfit(Surv(feed,stopp) ~ econ, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

km <- survfit(Surv(feed,stopp) ~ smok, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

km <- survfit(Surv(feed,stopp) ~ alco, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

km <- survfit(Surv(feed,stopp) ~ care, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

head(bb,2)
range(bb$feed)
range(bb$age)
hist(bb$age)
lines(c(mean(bb$age),mean(bb$age)), c(0, 120),  col="red")
wiek22 <- ifelse(bb$age<22,1,0)
bbb <- cbind(bb[,-6],wiek22)
head(bbb,2)

km <- survfit(Surv(feed,stopp) ~ wiek22, data=bb, 
              conf.type="none")
plot(km,col=c("red","blue"), lty=1:2)

# wyglada na to, ze nic nie roznicuje tutaj...

################### PH ###################################################

ph <- coxph(Surv(feed, stopp)~., data=bbb)
summary(ph)
AIC(ph)
ph2 <- coxph(Surv(feed, stopp)~econ+smok+educ+rasa_biala+wiek22, data=bbb) 
summary(ph2)
AIC(ph2)
anova(ph2,ph)  # ten mniejszy lepszy 

summary(ph2)

test_shoen <- cox.zph(ph2, transform="identity")
test_shoen   # spelnione zalozenie prop. hazardow

############# patrzymy na rownoleglosc:

kme <- survfit(Surv(feed,stopp) ~ econ, data=bbb, 
               conf.type="none")
plot(kme, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kms <- survfit(Surv(feed,stopp) ~ smok, data=bbb, 
               conf.type="none")
plot(kms, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kmed <- survfit(Surv(feed,stopp) ~ educ, data=bbb, 
                conf.type="none")
plot(kmed, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kmr <- survfit(Surv(feed,stopp) ~ rasa_biala, data=bbb, 
               conf.type="none")
plot(kmr, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)


########################## marcin kopiuje wkleja #######
pustyph <- coxph(Surv(feed, stopp)~1, data=bb) 
mart <- resid(pustyph)

plot(bbb$educ,mart, xlab="educ", ylab="Reszty martynga這we",pch=19) 
lines(lowess(bbb$educ,mart,iter=0,f=0.6), col="orange") 


plot(bb$age,mart, xlab="age", ylab="Reszty martynga這we",pch=19) 
lines(lowess(bb$age,mart,iter=0,f=0.6), col="orange") 







#########################  ogolnie to te rzeczy wszystkie 
#########################  nie maja chyba zbyt wielkiego wplywy wcale...

summary(ph2)

############### moze jeszcze cos z ta edukacja zrobic...

mean(bbb$educ)
range(bbb$educ)
quantile(bbb$educ)
hist(bbb$educ)

eduk12 <- ifelse(bbb$educ<=12,0,1)
head(bbb)
bbb <- cbind(bbb[,-6],eduk12)

### zobaczmy teraz:

ph <- coxph(Surv(feed, stopp)~., data=bbb)
summary(ph)

ph2 <- coxph(Surv(feed, stopp)~econ+smok+eduk12+rasa_biala+wiek22, data=bbb) 
summary(ph2)

anova(ph2,ph)  # ten mniejszy lepszy 

summary(ph2)

test_shoen <- cox.zph(ph2, transform="identity")
test_shoen   # spelnione zalozenie prop. hazardow

############# patrzymy na rownoleglosc:

kme <- survfit(Surv(feed,stopp) ~ econ, data=bbb, 
               conf.type="none")
plot(kme, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kms <- survfit(Surv(feed,stopp) ~ smok, data=bbb, 
               conf.type="none")
plot(kms, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kmed <- survfit(Surv(feed,stopp) ~ eduk12, data=bbb, 
                conf.type="none")
plot(kmed, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

kmr <- survfit(Surv(feed,stopp) ~ rasa_biala, data=bbb, 
               conf.type="none")
plot(kmr, col=c("red","blue"),
     fun=function(x) log(-log(x)), log="x", firstx=1)

#########################  ogolnie to te rzeczy wszystkie 
#########################  nie maja chyba zbyt wielkiego wplywy wcale...

summary(ph2)


########################### teraz marcin diagnozuje model ##############
#### residua shenelda - projkt 2 , strona 3
bbb.PH  <- coxph(Surv(feed,stopp) ~ ., data=bbb) # model pelny coxa :)
bbb.PHfit.all <- cox.zph(bbb.PH, transform="identity") # test shnfelda 

#residua shenflda 
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=1, main="zmienna 1 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=2, main="zmienna 2 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=3, main="zmienna 3 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=4, main="zmienna 4 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=5, main="zmienna 5 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=6, main="zmienna 6 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=7, main="zmienna 7 ", pch=19)
abline(0, 0, lty=3)
plot(bbb.PHfit.all, df=3, nsmo=10, se=TRUE, var=8, main="zmienna 8 ", pch=19)
abline(0, 0, lty=3)

## ogolne dopasowanie modelu , proj2 , str 4, wykresy reszt deiwancji i 
## reszt martyngalowcyh



bbb.devres1.all <- residuals(bbb.PH,type="deviance")
bbb.fitval1.all <- bbb.PH$linear.predictors
mart <- residuals(bbb.PH)
   

plot(1:length(bbb.devres1.all),bbb.devres1.all, pch=19, xlab="index", ylab="reszty dewiancji")
abline(0,0, col="orange", lwd=3)

plot(bbb.fitval1.all,bbb.devres1.all, pch=19, xlab="liniowa komb zmiennych", ylab="reszty dewiancji")
abline(0,0, col="orange", lwd=3)

plot(1:length(mart),mart, pch=19, xlab="index", ylab="reszty martynga這we")
abline(0,0, col="orange", lwd=3)

plot(bbb.fitval1.all,mart, pch=19, xlab="liniowa komb zmiennych", ylab="reszty martynga這we")
abline(0,0, col=colors()[sample(1:657, 1)], lwd=3)


