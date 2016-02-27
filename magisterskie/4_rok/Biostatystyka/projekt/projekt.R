library("foreign")
library("survival")

################### przygotowanie danych ################################

b <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\projekt\\BreastFeeding.dta")
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

mean(bb$age)
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

ph2 <- coxph(Surv(feed, stopp)~econ+smok+educ+rasa_biala+wiek22, data=bbb) 
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

kmed <- survfit(Surv(feed,stopp) ~ educ, data=bbb, 
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

