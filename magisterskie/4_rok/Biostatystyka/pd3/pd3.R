# 1

b3 <- read.csv("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\pd3\\bladder3.csv")
head(b3)

# enum i rx-y sa zalezne, wiec enum usuwamy

mod1 <- coxph(Surv(time1,time2,status) ~ rx1 + rx2 + rx3 + rx4 + size + number + cluster(patid),
              data=b3)
summary(mod1)

# leczenie to jeden - placebo albo dwa - cos tam
# rx1 wyszlo istotne tzn., ze jesli jestesmy w czasie ryzyka pierwszej wznowy, to ryzyko, ze ja miec
# zmniejsza efekt leczenia o polowe

zal_mod1 <- cox.zph(mod1, transform="identity")
zal_mod1   # zalozenia modelu spalnione (z testu skalowanych reszt Schoenfelda)

#########################################################################################
# 2

l <- read.csv("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\pd3\\leukemia.csv")
head(l)

mod <- coxph(Surv(time,status) ~ treat + frailty(patid)+strata(partial), data=l)
summary(mod)

?coxph
# 3

m <- read.csv("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\pd3\\mammary.csv")
head(m)

mod <- coxph(Surv(time1,time2,status) ~ treat + cluster(ratid), data=m)
summary(mod)

dd


# 4

library("cmprsk")

m <- read.csv("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\pd3\\hodgkin1.csv")
head(m)
m <- m[,-2]
head(m)

ci <- survfit(Surv(firsttime, event) ~1 , etype=event_cause , data = m)

   plot(ci, mark.time=FALSE, conf.int=FALSE, lty=c(2,0), col=2, xscale=1,
        xmax=13, xlab="Years post-HIV infection")
lines(ci, fun="event", mark.time=FALSE, conf.int=FALSE, lty=c(0,2), col=3,
      xmax=13)
text(8, .8, "1-CI, AIDS", col=2)
text(8, .2,"CI, SI", col=3)
attach(m)
ci.cmprsk <- cuminc(ftime=firsttime,fstatus=event_cause)
print(ci.cmprsk,ntp=2)

cmprsk:::plot.cuminc(ci.cmprsk,lty=2,c=2:3,xlab="Years post-HIV infection", ylab="CIF",
                         curvlab= c("AIDS","SI"))
?plot.cmprsk
head(m)
head(dane)
ci.ccr5 <- cuminc(firsttime,event_cause,group=cmt)
cmprsk:::plot.cuminc(ci.ccr5,lty=2,c=2:3,xlab="Years post-HIV infection", ylab="CIF",
                     curvlab= c("AIDS","SI"))

##################################3

ci.sfit <- survfit(Surv(firsttime, event) ~ cmt, etype=event_cause , data = m)

plot(ci.sfit, fun="event", lty=c(1,0,2,0), col=2, mark.time=FALSE,
        conf.int=FALSE, xscale=1, xmax=13, xlab="Years post-HIV infection")
text(6, .275, "WW, AIDS", col=2)
text(6, .075, "WM, AIDS", col=2)

Argument lty=c(1,0,2,0) pozwala na „wygaszenie” wykresu sub-dystrybuant dla SI.
Podobnie dla SI:
   plot(ci.sfit, fun="event", lty=c(0,1,0,2), col=3, mark.time=FALSE,
        conf.int=FALSE, xscale=1, xmax=13, xlab="Years post-HIV infection")
text(6, .275, "WW, AIDS", col=3)
text(8, .225, "WM, AIDS", col=3)

#####################################################
# od poczatku:

library("cmprsk")

m <- read.csv("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\pd3\\hodgkin1.csv")
head(m)
attach(m)

summary(coxph.inny <- coxph(Surv(firsttime, event_cause == 1) ~ cmt, data=m))
summary(coxph.hod <- coxph(Surv(firsttime, event_cause == 2) ~ cmt, data=m))

cox.zph(coxph.inny, transform="identity")
cox.zph(coxph.hod, transform="identity")

mac <- model.matrix(~ cmt + male + nodes + mediast + age30 + clinstg)[,-1]

summary(mod.inny <- crr(ftime=firsttime, fstatus=event_cause, cov1=mac,
                        failcode=1))
summary(mod.hod <- crr(ftime=firsttime, fstatus=event_cause, cov1=mac,
                        failcode=2))

inny.pred <- predict(mod.inny, cov1=rbind(0,1))
plot(inny.pred,lty=1:2,color=2:3, main="Inny")
legend("topleft",lty=1:2,col=2:3,c("radio","radio+chemio"),cex=0.7)

hod.pred <- predict(mod.hod, cov1=rbind(0,1))
plot(hod.pred,lty=1:2,color=2:3, main="Hod")
legend("bottomright",lty=1:2,col=2:3,c("radio","radio+chemio"),cex=0.7)
