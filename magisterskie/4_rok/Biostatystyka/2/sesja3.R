library("foreign")
library("survival")
library("rms")

brack <- read.dta("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\Biostatystyka\\2\\brackets_eng.dta")
head(brack)

brack.KM.mat <- survfit(Surv(pascal, ind) ~ mat, data=brack)
plot(brack.KM.mat,col=c("red","blue","green"))
plot(brack.KM.mat, col=c("red","blue","green"),
     fun=function(x) log(-log(x)), log="x",firstx=1)
# nie sa rownolegle :( -> zalozenie raczej nie spelnione

brack.KM.term <- survfit(Surv(pascal, ind) ~ term, data=brack)
plot(brack.KM.term,col=c("red","blue","green"))
plot(brack.KM.term, col=c("red","blue","green"),
     fun=function(x) log(-log(x)), log="x",firstx=1)
# tez nie sa rownolegle

brack.KM.comb <- survfit(Surv(pascal, ind) ~ group, data=brack)
plot(brack.KM.comb,col=1:6)
plot(brack.KM.comb, col=1:6,
     fun=function(x) log(-log(x)), log="x",firstx=1)
# niespelnione zalozenia modelu PH

# model Weibulla:

brack.AFT.Weib <- survreg(Surv(pascal, ind) ~ term + mat1 + mat2 + termmat1
           + termmat2, data=brack, dist="weibull")
print(brack.AFT.Weib)
p <- 1/brack.AFT.Weib$scale
p
time.ratio.Weib <- exp(brack.AFT.Weib$coefficients)
time.ratio.Weib

# miara dopasowania:

brack.AFT.Weib1 <- psm(Surv(pascal, ind) ~ term + mat1 + mat2 + termmat1 +
          termmat2, data=brack, dist="weibull", y=TRUE)
res.Weib1 <-resid(brack.AFT.Weib1,type="cens")

plot(survfit(res.Weib1~1),conf="none",
         ylab="Survival probability (Gumbel)", xlab="Residual")
lines(res.Weib1, col="red")

brack.KM.comb1 <- survfit(Surv(pascal, ind) ~ group, data=brack,
           subset=(group==1))
plot(brack.KM.comb1, conf="none", lty=2)
survplot(brack.AFT.Weib1,term=0,mat1=0,mat2=0,termmat1=0,termmat2=0,add=TRUE)


