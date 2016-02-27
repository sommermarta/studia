#7.16
p<-c(145,150,153,148,141,152,146,154,139,148)
d<-c(152,150,147,155,140,146,158,152,151,143,153)

srp<-mean(p)
srd<-mean(d)
srp
srd
np<-length(p)
nd<-length(d)
np
nd
wp<-var(p)
wd<-var(d)
wp
wd
L<-srp-srd
Ll<-(np-1)*wp+(nd-1)*wd
M<-np+nd-2
N<-(np+nd)/(np*nd)

T<-L/sqrt(Ll*N/M)
T
M



#17
(2210-2280)/sqrt(10000/100+6400/90)
qnorm(0.99)

#18
p<-c(40.32,39.85,41.17,40.62,40.04)
d<-c(51.07,49.60,50.45,50.59,50.29)
n<-5

sp<-var(p)
sd<-var(d)

t<-sp/sd
t
var.test(p,d,0.05)
qf(0.975,4,4)
#19

u<-c(23,18,22,21,25,24)
f<-c(17,22,19,18,20,19,23)
sf<-var(f)
su<-var(u)
t<-sf/su
t
var.test(f,u,0.05)
df(1-0.05,4,3)