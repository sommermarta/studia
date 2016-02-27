?rle
z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
rle(z)
rle(z)$lengths


x<-c(2,3,4,5,6,7,8,9,5,5,5,5,5,6,6,8,8,8,8,5,8,8,8,8,6,8,7,2,2,2,5,6,5)
y <- sort(x); y
rle(y)
l <- rle(y)$lengths
v <- rle(y)$values

s <- which(l==max(l))
s
t <- v[s[1]]
t

#zad1.8

x <- c(TRUE, FALSE, FALSE, FALSE, NA, TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE)
y <- na.omit(x)
y
n <- length(y)
which(y==TRUE)
k <- length(which(y==TRUE))
k

beta <- 0.05
q <- qnorm(1-beta/2, 0, 1); q
l <- q-sqrt((k*(n-k))/n^3); l
p <- q+sqrt((k*(n-k))/n^3); p

abs(l-p)


#1.6

x<-c(2,3,4,5,6,7,8,9,5,5,5,5,5,6,6,8,8,8,8,5,8,8,8,8,6,8,7,2,2,2,5,6,5)
y <- sort(x); y

#1.9
x <- c(2,3,4,2,2,4,4,3)
y <- c(3,5,6,5,4,3,2,2)
n <- length(x)   
n
rx<-rank(x)
ry<-rank(y)

d<-rx-ry
d

ro <- 1 - (6*sum(d^2))/(n*(n^2-1))
ro

?pt
dyst <- pt()
t <- 1 - pt(ro*sqrt((n-2)/(1-ro^2)),n-2)
t

#1.13

top <- c(" _ ","  "," _ "," _ ","   "," _ "," _ "," _ "," _ "," _ ")
mid <- c("| |"," |"," _|"," _|","|_|","|_ ","|_ ","  |","|_|","|_|")
bot <- c("|_|"," |","|_ "," _|","  |"," _|","|_|","  |","|_|"," _|")


cat(top); cat("\n"); cat(mid); cat("\n"); cat(bot)

x<-c(3,5,2,1,2,2,2,0)
y<-x+1
y
cat(top[y]); cat("\n"); cat(mid[y]); cat("\n"); cat(bot[y])


#zad 1.14

n1 <- 0:100
n1
p1 <- sum((4*(-1)^n1)/(2*n1+1))
p1

n2 <- 0:1000
n2
p2 <- sum((4*(-1)^n2)/(2*n2+1))
p2

n3 <- 0:10000
n3
p3 <- sum((4*(-1)^n3)/(2*n3+1))
p3

n <- c(p1,p2,p3)
n

eps <- n-pi
eps

#11

x <- round(runif(10,1,10))
y <- round(runif(10,1,10))
m <- min(nx,ny)

tx <- tabulate(x,nbins=m)
ty <- tabulate(y,nbins=m)
# nx <- length(tx)
# ny <- length(ty)
# 
# 
# r <-max(nx,ny)-min(nx,ny) 
# rr <- numeric(r)

# txr <- c(tx,rr)
# tyr <- c(ty,rr)
tabulate(x); tabulate(y)
x; y; tx; ty
b <- (which(tx!=0 & ty!=0))
x; y
b
intersect(x,y)

#7

x <- c("a","b","c","d","e","f","g")
k <- round(runif(1,1,25))
k
y <- round(runif(k,1,length(x)))
y
x[y]

#6
x <- c(1,2,1,4,3,4,1)
y <- sort(x)
r <- rle(y)$values
r
which(x[]==r[])


#10

n <- 1:10
#n
x <- runif(1,-1,1)
x
as <- sum((factorial(2*n)*x^(2*n+1))/((4^n)*((factorial(n))^2)*(2*n+1)))
as
asin(x)

n <- 1:100
typeof(n)
j<-as.double(n)
typeof(j)
#n
#x <- runif(1,-1,1)
#x
as <- sum((factorial(2*n)*x^(2*n+1))/((4^n)*((factorial(n))^2)*(2*n+1)))
as
warnings()

factorial(2*100.00)



#8
x <- c(TRUE, FALSE, FALSE, FALSE, NA, TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE)
y <- na.omit(x)
y
n <- length(y)
n
beta <- runif(1,0,1)
beta
?runif
alfa <- 1-beta
alfa
kk <- which(y==TRUE)
kk
k <- length(kk)
k
#clop

a1 <- alfa/2
a2 <- 1-alfa/2

bl <- pbeta(a1 , k , n-k+1)
bp <- pbeta(a2 , k+1 , n-k) 

bl; bp
bp-bl

#asym

q <- qnorm(a2, 0, 1)
p <- sqrt(k*(n-k)/(n^3))

al <- q-p
ap <- q+p

al; ap
ap-al




#12

x <- c(1,5,6,9,7,3,5,6,4) 
y <- 3
n <- length(x)

z <- as.numeric(!findInterval(x,y, rightmost.closed=T))
l <- sum(z)
l
f <- l/n
f





#9
x <- c(2,3,4,2,2,4,4,3)
y <- c(3,5,6,5,4,3,2,2)
n <- length(x)   
n
rx<-rank(x)
ry<-rank(y)

d<-rx-ry
d

ro <- 1 - (6*sum(d^2))/(n*(n^2-1))
ro

?pt
t <- 1 - pt(ro*sqrt((n-2)/(1-ro^2)),n-2)
t



cumsum(1:5)
x <- c(1,2,1,4,3,4,1); x
y <- sort(x); y
v <- rle(y)$values; v
l <- rle(y)$lengths; l
cl <- cumsum(l); cl
y[cl]

cummin(1:5)
?cummin













