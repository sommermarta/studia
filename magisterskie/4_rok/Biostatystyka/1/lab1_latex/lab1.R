library("foreign")
library("survival")

# 1)

seasick <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\1\\seasick_eng_data.dta")
head(seasick)

# dla pierwszego eksperymentu:

sea1.KM <- survfit(Surv(time, vomit) ~ intens, data=seasick,
                   conf.type="none",subset=intens==1)
summary(sea1.KM)
plot(sea1.KM, col="red",lty=2,xlab="minuty",ylab="p-stwo przezycia")

# dla obu eksperymentow:

sea.KM <- survfit(Surv(time, vomit) ~ intens, data=seasick, conf.type="none")
summary(sea.KM)

plot(sea.KM, col=c("red","blue"), lty=c(1,2), xlab="minuty",
     ylab="p-stwo przezycia")
legend(5,.3,c("pierwsze dosw.","drugie dosw."),
       col=c("red","blue"), lty=c(1,2),cex=0.7)

# porowynywanie krzywych przezycia:

sea.test <- survdiff(Surv(time, vomit) ~ intens, data=seasick, rho=0)
sea.test

# p-value duze, zatem przyjmujemy hipoteze, czyli krzywe przezycia nie 
# roznia sie istotnie

# 2)

nsclc <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\1\\nsclc_eng.dta")
head(nsclc)

nsclc.KM <- survfit(Surv(survtime,survind)~expression,data=nsclc,
                    conf.type="none")

plot(nsclc.KM,col=c("red","blue"), lty=c(1,2), xlab="miesiace",
     ylab="p-stwo przezycia")
legend(.4,.3,c("brak ekspresji bialka","ekspresja bialka"),
       col=c("red","blue"), lty=c(1,2),cex=0.7)

nsclc.test <- survdiff(Surv(survtime,survind)~expression,data=nsclc)
nsclc.test    # krzywe roznia sie istotnie

# test warstwowy:

nsclc.test.w <- survdiff(Surv(survtime,survind)~expression+strata(tnm)
                         ,data=nsclc)
nsclc.test.w

# wykresy dla warstw oddzielnie:

nsclc.KM.1 <- survfit(Surv(survtime,survind)~expression,data=nsclc,
                      conf.type="none",subset=tnm==1)
plot(nsclc.KM.1,col=c("red","blue"), lty=c(1,2), xlab="miesiace",
     ylab="p-stwo przezycia",main="TNM = 1")
legend(.4,.3,c("brak ekspresji bialka","ekspresja bialka"),
       col=c("red","blue"), lty=c(1,2),cex=0.7)

nsclc.KM.2 <- survfit(Surv(survtime,survind)~expression,data=nsclc,
                      conf.type="none",subset=tnm==2)
plot(nsclc.KM.2,col=c("red","blue"), lty=c(1,2), xlab="miesiace",
     ylab="p-stwo przezycia",main="TNM = 2")
legend(.4,.3,c("brak ekspresji bialka","ekspresja bialka"),
       col=c("red","blue"), lty=c(1,2),cex=0.7)

nsclc.KM.3 <- survfit(Surv(survtime,survind)~expression,data=nsclc,
                      conf.type="none",subset=tnm==3)
plot(nsclc.KM.3,col=c("red","blue"), lty=c(1,2), xlab="miesiace",
     ylab="p-stwo przezycia",main="TNM = 3")
legend(.4,.3,c("brak ekspresji bia³ka","ekspresja bialka"),
       col=c("red","blue"), lty=c(1,2),cex=0.7)



