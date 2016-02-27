###########################################################################
#2.5

sort_list <- function(x,f){
   stopifnot(is.list(x))
   stopifnot(is.function(f))
   stopifnot(is.numeric(unlist(x)))
   xa <- lapply(x,f)
   xu <- unlist(xa)
   xo <- order(xu)
   as.list(x[xo])
}

l <- list(3,1:4,c(3,8,6,5),1,c(3,3,3,3,3,3))
sort_list(l,mean)

typeof(sqrt)
typeof(mean)
typeof(sd)
typeof(sum)
class(mean)
class(sqrt)
length(unlist(l))

###########################################################################
# 2.8


factorize <- function(x){
   
   stopifnot(is.character(x))
   u <- union(x,x)
   lu <- length(u)
   lx <- length(x)
   
   c <- rep(u,each=lx)
   m <- rep(x, lu)
   
   a <- which(m == c)
   mmmmmm <- which(is.na(m)==TRUE & is.na(c)==TRUE)
   aa <- sort(c(a,mmmmmm))
   
   j <- mmmmmm%%lx
   j[which(j==0)] <- lx
   
   amodulo <- aa%%lx
   
   amodulo[which(amodulo==0)] <- lx
   
   sna <- sort(x, na.last=TRUE)
   r <- rle(sort(x))
   ss <- sum(is.na(sna))
   
   hi <- c(r$lengths,ss)[order(order(u))]
   
   n <- rep(1:lu,times=hi)
   
   x[amodulo] <- n
   x[j] <- NA
   x <- as.numeric(x)
   takibedziena <- which(is.na(u)==TRUE)
   x[which(x>takibedziena)] <- x[which(x>takibedziena)] - 1
   
   list(u,x)
   
}

w <- c("ala","ala","zosia","ala","kasia","ala","zosia","ala","ala","zosia",
       "elwira","bozena","bozena")
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]

x <- c("ala",NA,"ala","zosia","ala","kasia","ala","zosia","ala","ala","zosia",
       "elwira","bozena","bozena")
y1<-unique(x) 
y1
y2<-charmatch(x,y1) 
y2
list(y1,y2)





w <- c("mam", "tak", "134","samo", "mam", "88iiii","tak", "samo", "jestem", 
       "tu", "!", "siabadaba", "234")
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]


w <- c("mam", "tak","samo", "mam","tak", "samo", "jestem", 
       "tu", "siabadaba")
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]


w <- c("ala","ala","zosia","ala","kasia","ala","zosia")
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]
wynik

w <- c("ala","ala","zosia","ala","kasia","ala","zosia", NA)
wynik <- factorize(w)
wynik[[1]][wynik[[2]]]

x <- c("ala",NA,"ala",NA,NA,NA,"YYYYYYYY","JHGADKHFD","zosia","ala","kasia",
       "ala","zosia", NA, "5555")
factorize(x)

x <- c("agr", "33", "agr",33,list(1:9))
factorize(x)

###########################################################################
#2.11

chrapaniesynchroniczne <- function(x,y){
   
   n <- length(x)
   i <- 1:n
   
   nn <- rep(1:90,each=n)
   xx <- numeric(90)
   xx[1:n] <- x
   
   a <- y[i] + xx[i]*nn
   
   aa <- 1:(n*90)
   aaa <- aa%%n
   aaa[which(aaa==0)] <- n
   
   aaaa <- rep(aaa,n)
   bbbb <- rep(1:n,each=n*90)
   
   z <- which(aaaa==bbbb)
   zz <- z%%(n*90)
   zz[which(zz==0)] <- n*90
   
   mm <- a[zz]
   m <- mm[-which(mm>90)]
   
   s <- sort(m)
   r <- rle(s)
   w <- which(r$lengths==n)
   
   as.logical(length(w))
}


x <- c(6,3,10,3,90,444)
y <- c(0,15,30,0,0,99)
chrapaniesynchroniczne(x,y)

##########################################################################
#2.10

approxinvert <- function(f,y,a,b,k=100L){
   
   stopifnot(y>=f(a) & y<=f(b))
   stopifnot(is.double(a))
   stopifnot(is.double(b))
   stopifnot(length(a)==1 & length(b)==1)
   stopifnot(a<b)
   stopifnot(k>2 & is.integer(k))
   kk <- seq(a,b,length.out=k)
   a <- approxfun(f(kk),kk)
   a(y)
}


f <- function(x){
   x+2
}
y <- c(-2,3,4,4.44)
approxinvert(f,y,-4,5)

###########################################################################

#2.9
mu <- 3
sigma <- 0.56
n <- 1100

n2 <- ceiling((n+1)/2)
n2
n2*2

u1 <- runif(n2,0,1)
u2 <- runif(n2,0,1)


lo <- sqrt(-2*log(u1))
po <- 2*pi*u2
z1 <- lo*cos(po)
z2 <- lo*sin(po)

zz <- c(z1,z2)
zz

c <- ceiling(runif(length(zz)-n,0,length(zz)))
zz[-c]


z <- mu +sigma*zz[-c]
z
mean(z)
var(z)
sigma^2



#########################################################################
#2.6

remove_el_types <- function(x,type){
   
   stopifnot(is.list(x))
   stopifnot(is.character(type))
   
   lap <- lapply(x,typeof)
   nielist <- unlist(lap)
   l <- length(nielist)
   ord <- order(nielist)
   sor <- sort(nielist)
   r <- rle(sor)
   val <- r$values
   len <- r$lengths
   lval <- length(val)
   llen <- length(len)
   s <- charmatch(type,val)
   ls <- length(s)
   
   re <- rep(1:lval,len)
   licz <- re[order(ord)]
   liczcz <- rep(licz, ls)
   rere <- rep(s,each=l)
   a <- which(rere==liczcz)
   aa <- a %% l
   aa[which(aa==0)] <- l
   
   x[-aa]
}

x <- list("sg",76,98,"ajhg")
y <- c( "logical","integer")
remove_el_types(x,y)

x <- list("adfsfa",75L,c(235,346,"sf"),83475,TRUE,"gr","reyera")
y <- c("double", "integer","character", "logical")
remove_el_types(x,y)

x <- list("adfsfa",75L,c(235,346,"sf"),83475,TRUE,"gr","reyera",NA)
y <- c( "logical","integer")

lap <- lapply(x,typeof)
lap   
nielist <- unlist(lap)
nielist   
charmatch(nielist,y)
x[-which(charmatch(nielist,y)>=1)]   


###########################################################################
#2.9

u1 <- runif(100,0,1)
u2 <- runif(100,0,1)
z1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
z2 <- sqrt(-2*log(u1))*sin(2*pi*u2)
n<-21

rnorm2 <- function(n,mu,sigma){
   u1 <- runif(n,0,1)
   u2 <- runif(n,0,1)
   lo <- sqqrt(-2*log(u1))
   po <- 2*pi*u2
   z1 <- lo*cos(po)
   z2 <- lo*sin(po)
   
   z1*(n%%2==0)
   z2*(n%%2==1)
   z <- mu +sigma*c(z1,z2)*c(n%%2==0,n%%2==1)
   z
}


mu <- 3
sigma <- 0.56
n <- 100

n2 <- ceiling((n-1)/2)
n2

u1 <- runif(n2,0,1)
u2 <- runif(n2,0,1)
lo <- sqrt(-2*log(u1))
po <- 2*pi*u2
z1 <- lo*cos(po)
z2 <- lo*sin(po)

zz <- c(z1,z2)
zz

z <- mu +sigma*zz
z
mean(z)
var(z)
sigma^2















##########################################################################

#2.7

w <- c(0.15,0.36,0.12,0.33,0.04)
sum(w)
n <- length(w)
n
a <- 1:(n-1)
a
b <- a
b

c <- a
d <- which(a>a[b])
d

i <- 1:4
i

b <- vector("list",n-2)
b
b[[1]] <- i[-1]
b[[2]] <- i[-(1:2)]
b[[3]] <- i[-(1:3)]

a <- vector("list",n-2)
a
a[[1]] <- 1:1
a[[2]] <- 1:2
a[[3]] <- 1:3

a <- 1:4
b <- 1:4
a; b
c <- list(a[1],b[i])
c


##########################################################################



x <- list("adfsfa",75L,c(235,346,"sf"),83475,TRUE,"gr","reyera",NA)
y <- c( "logical","integer")
   
lap <- lapply(x,typeof)
lap   
nielist <- unlist(lap)
nielist   
charmatch(nielist,y)
x[-which(charmatch(nielist,y)>=1)]   

x<-list(1,3:4,c("ala","kota"),TRUE) 
y <- c("double","complex")
x

typeof(NA)




l <- length(nielist)
   ord <- order(nielist)
   sor <- sort(nielist)
   r <- rle(sor)
   val <- r$values
   len <- r$lengths
   lval <- length(val)
   llen <- length(len)
   s <- charmatch(type,val)
   ls <- length(s)
   
   re <- rep(1:lval,len)
   licz <- re[order(ord)]
   liczcz <- rep(licz, ls)
   rere <- rep(s,each=l)
   a <- which(rere==liczcz)
   aa <- a %% l
   aa[which(aa==0)] <- l
   
   x[-aa]
}


x <- list("adfsfa",75L,c(235,346,"sf"),83475,TRUE,"gr","reyera")
y <- c("double", "integer","character", "logical")
remove_el_types(x,y)


source('http://static.rexamine.com/packages/stringi_install.R')


