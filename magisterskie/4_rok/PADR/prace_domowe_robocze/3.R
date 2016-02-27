# 3.7

rle2 <- function(x){
   
   l <- numeric(length(x))
   v <- numeric(length(x))
   s <- 1
   a <- 0
   
   for(i in 1:length(x)){
      
      if(i==length(x)){
         
         v[i-a] <- x[i-s+1]
         l[i-a] <- s
         
      } else{
         
         if(x[i]==x[i+1]){
            
            s <- s+1
            a <- a+1
            
         } else{
            
            if(s==1){
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- 1
               
            } else{
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- s
               
            }
            
            s <- 1
         }
      }
   }
   
   c <- length(x)-a+1
   v <- v[-(c:length(v))]
   l <- l[-(c:length(l))]
   rle2 <- list(lengths=l,values=v)
   class(rle2) <- "rle"
   rle2
   
}

x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,9,4)
rle(x)
rle2(x)

######################################################################

# 3.8

is.prime <- function(x){
   
   a <- logical(length(x))
   
   for(i in (1:length(x))){
      
      if( any( ( (x[i] %% 2:(x[i]-1) ) == 0 )) ){
         
         a[i] <- FALSE
         
      } else{
         
         a[i] <- TRUE
      }
   }
   
   a
}

is.prime2 <- function(x){
   
   f <- function(i){
      
      any( ( (x[i] %% 2:(x[i]-1) ) == 0 ))
      
   }
   
   !unlist(lapply(1:length(x),f))
}

x <- c(3,4,7,7,11,4,5,25,31,30,1,0)
is.prime(x)
is.prime2(x)

install.packages("microbenchmark")
library("microbenchmark")
microbenchmark(is.prime(x),is.prime2(x))


########################################################################

# 3.11

uzubraki <- function(x){
   
   a <- as.numeric(is.na(x))
   s <- 1
   u <- numeric(length(x))
   ra <- rle(a)
   rl <- ra$lengths
   lrl <- length(rl)
   rv <- ra$values
   if(rv[1]==1) k <- rl[1] else k <- 1
   
   for(i in 1:(length(x))){
      
      if(i==1){
         if(a[1]==0) u[1] <- x[1] else{
            w <- rep(x[1+rl[1]],rl[1])
            u[1:rl[1]] <- w
         }
      }
      
      if(i==length(x)){
         if(a[i]==0) u[i] <- x[i] else{
            w <- rep(x[i-rl[lrl]],rl[lrl])
            u[length(x):(length(x)-rl[lrl]+1)] <- w
         }
      }
      
      if(all(i!=1:k & i!=length(x))==TRUE){
         if(a[i]==0){
            u[i] <- x[i] 
         }  else{
            if(a[i+1]==1){
               s <- s+1
            } else{
               m <- (x[i+1]-x[i-s])/(s+1)
               mm <- seq(x[i-s],x[i+1],by=m)
               mm <- mm[-c(1,length(mm))]
               u[(i-s+1):i] <- mm
               s <- 1
            }
         }
      }
   }
   
   round(u,2)
   
}


x <- c(1,NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13)
uzubraki(x)

x <- c(NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13)
uzubraki(x)

x <- c(NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA)
uzubraki(x)

x <- c(2,3,5,10,13)
uzubraki(x)


##########################################################################

#3.12


merge2 <- function(x,y){
   
   nx <- names(x)
   ny <- names(y)
   iu <- intersect(nx,ny)
   u <- union(nx,ny)
   su <- sort(u)
   
   f <- function(i){
      c(x[[i]],y[[i]])
   }
   
   ff <- function(i){
      typeof(x[[i]])==typeof(y[[i]])
   }
   
   li <- lapply(su,f)
   names(li) <- su
   
   li2 <- unlist(lapply(iu,ff))
   if(any(li2 == FALSE))  warning("Nastapila koercja typow!")
   
   li
   
}

x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=TRUE)

merge2(x,y)

#########################################################################

# 3.13

gendyskr <- function(n,x,p){
   
   stopifnot(n>=1)
   stopifnot(floor(n)==n)
   stopifnot(is.numeric(x))
   stopifnot(unique(x)==x)
   stopifnot(length(x)>=1)
   stopifnot(length(x)==length(p))
   stopifnot(p<=1 & p>=0)
   
   if(sum(p)!=1){
      
      p <- p + (1-sum(p))/length(p)
      warning("Wektor p zostal unormowany!")
   } 
   
   
   f <- function(i){
      u <- runif(1)
      c <- cumsum(p)
      m <- which(c>=u)[1]
      x[m]
   }
   
   u <- unlist(lapply(1:n,f))
   u
   
}

x <- c(5,6,7,2,1,4,8)
p <- c(0.2,0.2,0.3,0.11,0.09,0.05,0.05)
gendyskr(20,x,p)

rle(sort(gendyskr(20,x,p)))


##########################################################################


#3.10


logiderle <- function(i,j,n){
   
   stopifnot(is.numeric(i) & is.numeric(j))
   stopifnot(floor(i)==i & floor(j)==j)
   stopifnot(length(i)=length(j))
   stopifnot(n>=1 & floor(n)==n)
   stopifnot(all(i>=1) & all(i<=j) & all(j<=n) 
             & all(i[2:length(i)] > j[1:length(j)-1]))
   
   
   fun <- function(l){
      c(i[l],j[l])
   }
   
   li <- length(i)
   a <- lapply(1:li,fun)
   
   fff <- function(k){
      l <- 1:n
      l>=a[[k]][1] & l<=a[[k]][2] 
   }
   
   mmmm <- lapply(1:li,fff)
   q <- unlist(mmmm)
   
   t <- logical(n)
   t[which(q==TRUE) %% n] <- TRUE
   t
   
}



i <- c(1,4,7)
j <- c(1,6,7)
n <- 7

all(i>=1) & all(i<=j) & all(j<=n) & all(i[2:length(i)] > j[1:length(j)-1])

  
   fun <- function(l){
      c(i[l],j[l])
   }
   
   li <- length(i)
   a <- lapply(1:li,fun)
a   

   fff <- function(k){
      l <- 1:n
      l>=a[[k]][1] & l<=a[[k]][2] 
   }
   
   mmmm <- lapply(1:li,fff)
mmmm   
q <- unlist(mmmm)
   q
   t <- logical(n)
w <- which(q==TRUE) %% n
w[which(w==0)] <- n
w
t[w] <- TRUE
t[which(q==TRUE) %% n] <- TRUE
   t


###########################################################################


x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=TRUE)
x;y
   
   nx <- names(x)
   ny <- names(y)
   u <- union(nx,ny)
   su <- sort(u)
   
   f <- function(i){
      c(x[[i]],y[[i]])
   }
   
f("a")
f("b")
f("c")

   ff <- function(i){
      typeof(x[[i]])==typeof(y[[i]])
   }
  
ff("a")
ff("b")
ff("c")

su
u
iu <- intersect(nx,ny)

   li <- lapply(su,f)
   names(li) <- su
   
   li2 <- unlist(lapply(su,ff))
   if(any(li2 == FALSE))  warning("Nastapila koercja typow!")
   li
   


x <- list(a=1:3, c=c("e","f","g"))
y <- list(a=4:6, b=7:9, c=TRUE)


##########################################################################

x <- c(1,2,3,4)
   
   l <- numeric(length(x))
   v <- numeric(length(x))
   s <- 1
   a <- 0
   
   for(i in 1:length(x)){
      
      if(i==length(x)){
         
         v[i-a] <- x[i-s+1]
         l[i-a] <- s
         
      } else{
         
         if(x[i]==x[i+1]){
            
            s <- s+1
            a <- a+1
            
         } else{
            
            if(s==1){
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- 1
               
            } else{
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- s
               
            }
            
            s <- 1
         }
      }
   }
   
   c <- length(x)-a+1
if(!(c>length(x))){
   v <- v[-(c:length(v))]
   l <- l[-(c:length(l))]}
   rle2 <- list(lengths=l,values=v)
   class(rle2) <- "rle"
   rle2
   
}

x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,9,4)
rle(x)
rle2(x)


###########################################################################
uzubraki <- function(x){
   
   a <- as.numeric(is.na(x))
   s <- 1
   u <- numeric(length(x))
   ra <- rle(a)
   rl <- ra$lengths
   lrl <- length(rl)
   rv <- ra$values
   if(rv[1]==1) k <- rl[1] else k <- 1
   
   for(i in 1:(length(x))){
      
      if(i==1){
         if(a[1]==0) u[1] <- x[1] else{
            w <- rep(x[1+rl[1]],rl[1])
            u[1:rl[1]] <- w
         }
      }
      
      if(i==length(x)){
         if(a[i]==0) u[i] <- x[i] else{
            w <- rep(x[i-rl[lrl]],rl[lrl])
            u[length(x):(length(x)-rl[lrl]+1)] <- w
         }
      }
      
      if(all(i!=1:k & i!=length(x))==TRUE){
         if(a[i]==0){
            u[i] <- x[i] 
         }  else{
            if(a[i+1]==1){
               s <- s+1
            } else{
               m <- (x[i+1]-x[i-s])/(s+1)
               mm <- seq(x[i-s],x[i+1],by=m)
               mm <- mm[-c(1,length(mm))]
               u[(i-s+1):i] <- mm
               s <- 1
            }
         }
      }
   }
   
   round(u,2)
   
}


x <- c(1,NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13)
uzubraki(x)

x <- c(NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA,13)
uzubraki(x)

x <- c(NA,NA,NA,2,NA,3,NA,NA,5,10,NA,NA)
uzubraki(x)

x <- c(2,3,5,10,13)
uzubraki(x)





x <- c(NA,NA,1,NA,NA,5,10,NA,NA,13,NA,NA,NA)


uzubraki <- function(x){
   
   stopifnot(is.numeric(x))                  # x liczbowy
   stopifnot(sort(na.omit(x))==na.omit(x))    # x posortowany
   
   lx <- length(x)
   a <- as.numeric(is.na(x))
   r <- rle(a)
   rl <- r$lengths
   rv <- r$values
   lr <- length(rl)
   
   a1 <- numeric()
   a2 <- numeric()
   
   A <- numeric()
   B <- numeric()
   
   if(rv[1]==1){
      a1 <- 1:rl[1]
      A[a1] <- x[rl[1]+1]
      
   } 
   
   if(rv[lr]==1){
      a2 <- lx:(lx-rl[lr]+1)
      B[1:(rl[lr]+1)] <- x[lx-rl[lr]]
   }  
   
   a22 <- sort(a2)
   if(length(a1)+length(a22)==0) xx <- x else xx <- x[-c(a1,a22)]

   w <- which(as.numeric(is.na(rle(xx)$values))==0)
   lw <- length(w)
   
   f <- function(i){
      c(xx[w[i]],xx[w[i+1]])
   }
   
   przedz <- lapply(1:(lw-1),f)
   
   itjn <- diff(w)
   
   ff <- function(i){
      s <- seq(przedz[[i]][[1]],przedz[[i]][[2]],length.out=itjn[i]+1)
      s[-length(s)]
   }
   
   ost <- lapply(1:length(przedz), ff)
   ostt <- unlist(ost)
   
   if(length(B)==0) round(c(A,ostt,x[lx]),2) else round(c(A,ostt,B),2)
   
}

x <- c(NA,NA,1,NA,NA,5,10,NA,NA,13,NA,NA,NA)
aaa(x)

x <- c(1,NA,NA,5,10,NA,NA,13,NA,NA,NA)
aaa(x)

x <- c(NA,NA,1,NA,NA,5,10,NA,NA,13)
aaa(x)

x <- c(2,NA,6,8,9)
aaa(x)

x <- c(2,6,8,9)
aaa(x)


##########################################################################


x <- c(2,NA,6,8,9)

lx <- length(x)
a <- as.numeric(is.na(x))
a
r <- rle(a)
r
rl <- r$lengths
rv <- r$values
lr <- length(rl)

x2 <- numeric(lx)
a1 <- numeric()
a2 <- numeric()

A <- numeric()
B <- numeric()
if(rv[1]==1){
   a1 <- 1:rl[1]
   A[a1] <- x[rl[1]+1]
   
} 

if(rv[lr]==1){
   a2 <- lx:(lx-rl[lr]+1)
   B[1:(rl[lr]+1)] <- x[lx-rl[lr]]
}

A;B


a1
a22 <- sort(a2)
a22
length(a22)


if(length(a1)+length(a2)==0) xx <- x else xx <- x[-c(a1,a22)]
xx
rle(xx)
w <- which(as.numeric(is.na(rle(xx)$values))==0)
w
lw <- length(w)

f <- function(i){
   c(xx[w[i]],xx[w[i+1]])
}

przedz <- lapply(1:(lw-1),f)
przedz

itjn <- diff(w)
itjn

ff <- function(i){
   s <- seq(przedz[[i]][[1]],przedz[[i]][[2]],length.out=itjn[i]+1)
   s[-length(s)]
}

ff(1)
ff(2)
ff(3)

ost <- lapply(1:length(przedz), ff)
ostt <- unlist(ost)
ost

x2
A
B

if(length(B)==0) round(c(A,ostt,x[lx]),2) else round(c(A,ostt,B),2)
x


###########################################################################

x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,8,9,4)

lx <- length(x)

f1 <- function(i){
   x[i]==x[i+1]
}

a <- lapply(1:(lx-1),f)
ua <- as.numeric(unlist(a))
ua

w <- which(ua==1)
lw <- length(w)
w
sw <- sort(unique(c(w,w+1)))
sw
www <- which(diff(sw)-1!=0)
www
sw


   
lapply(www,g)


x[sw]


ff <- function(i){
   w[i]==w[i+1]-1
}

as <- as.numeric(unlist(lapply(1:(lw-1), ff)))
w

ww <- which(as==1)
ww

fff <- function(i){
   w[i:length(ww)]==
}

lapply(ww,fff)
















x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,8,9,4)



rle2 <- function(x){
   
   stopifnot(is.atomic(x))    # x - wektor atomowy
   stopifnot(length(x)>0)     # x nie moze byc pusty
   
   x <- na.omit(x)
   l <- numeric(length(x))
   v <- numeric(length(x))
   s <- 1
   a <- 0
   
   for(i in 1:length(x)){
      
      if(i==length(x)){
         
         v[i-a] <- x[i-s+1]
         l[i-a] <- s
         
      } else{
         
         if(x[i]==x[i+1]){
            
            s <- s+1
            a <- a+1
            
         } else{
            
            if(s==1){
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- 1
               
            } else{
               
               v[i-a] <- x[i-s+1]
               l[i-a] <- s
               
            }
            
            s <- 1
         }
      }
   }
   
   c <- length(x)-a+1    #tu usuwamy niepotrzebne koncowe elementy z v i l 
   
   if(!(c>length(x))){
      v <- v[-(c:length(v))]
      l <- l[-(c:length(l))]
   }
   
   rle2 <- list(lengths=l,values=v)
   class(rle2) <- "rle"
   rle2
   
}

## ---- Przyklady ----

# wiekszosc przykladow bede porownywac z funkcja rle() 
# - jesli jest ok, to powinno wychodzic TRUE

x <- c(2,4,6,2,2,4,6,"ddd","ddd","dd",88,TRUE,TRUE,8,8,8,9,4)
rle(x)
rle2(x)  
rle22(x)        


x <- c(2,4,6,8)
rle(x)
rle2(x)  
rle22(x)

x <- c()
rle2(x)     #error - wektor pusty

x <- c(2,4,6,8,NA)
rle2(x)
rle22(x)

x <- c(2,4,NA,6,8)
rle2(x)



rle22 <- function(x){
   
   lx <- length(x)
   
   f1 <- function(i){
      match(x,x[i])
   } 
   
   a <- lapply(1:(lx-1),f1)
   
   f2 <- function(i){
      which(a[[i]]==1)
   }
   
   la <- length(a)
   
   a2 <- lapply(1:la,f2)
   a3 <- lapply(a2,diff)
   
   f3 <- function(i){
      which(a3[[i]]==1)
   }
   
   a4 <- lapply(1:la,f3)
   
   f4 <- function(i){
      a2[[i]][a4[[i]]]
   }
   
   b <- lapply(1:la,f4)
   
   f5 <- function(i){
      b[[i]] +1 
   }
   
   bb <- lapply(1:la,f5)
   
   f6 <- function(i){
      sort(unique(c(b[[i]],bb[[i]])))
   }
   
   m <- lapply(1:length(b),f6)
   
   f7 <- function(i){
      x[m[[i]]]
   }
   
   mm <- lapply(1:la,f7)
   
   
   p <- unlist(lapply(mm,length))
   
   r <- rep(1,length(x))
   
   r[sort(unique(unlist(m)))] <- p[sort(unique(unlist(m)))]
   
   f8 <- function(i){
      all((c(r[i],x[i])==c(r[i+1],x[i+1])))==TRUE 
   }
   
   e <- unlist(lapply(1:(lx-1),f8))
   we <- which(e==TRUE) 
   r2 <- r[-we]
   x2 <- x[-we]
   
   li <- list(lengths=r2,values=x2)
   class(li) <- "rle"
   li
   
}


is.na(as.numeric("6"))
is.na(as.numeric("+"))
f <- get("+")
f(5,6)

s <- c(1,2,3)
s[-c(1,2)]



x <- c("1","2","+","3","*","4","5","+","6","7","+","/","-")

s <- numeric()
a <- 0

x <- c("*","+","-")

onp <- function(x){
   
   stopifnot(is.character(x))
   stopifnot(length(x)%%2!=0)
   stopifnot(sum(is.na(as.numeric(x)))==(length(x)-1)/2)
   
   s <- numeric()
   a <- 0
   
   for(i in 1:length(x)){
      
      if(is.na(as.numeric((x[i])))==FALSE){
         s[i-2*a] <- as.numeric(x[i])
      } else{
         f <- get(x[i])
         s[i-2*a] <- f(s[i-2*a-2],s[i-2*a-1])
         s <- s[-c(i-2*a-1,i-2*a-2)]
         a <- a+1
      }   
   }
   
   s
   
}

x <- c("1","2","+","3","*","4","5","+","6","7","+","/","-")
onp(x)

onp(c("2","3","+","1","/"))
onp(c(1,2,"+","-"))  #error
onp(c(12,2,3,4,"*",10,5,"/","+","*","+"))
onp(c(2,7,"+",3,"/",14,3,"-",4,"*","+",2,"/"))
onp(c("+","-","*"))

for(i in 1:length(x)){
   
   if(is.na(as.numeric((x[i])))==FALSE){
      s[i-2*a] <- as.numeric(x[i])
   } else{
      f <- get(x[i])
      s[i-2*a] <- f(s[i-2*a-2],s[i-2*a-1])
      s <- s[-c(i-2*a-1,i-2*a-2)]
      a <- a+1
   }   
}

s











x <- c("1","2","+","3","*","4","5","+","6","7","+","/","-")

s <- numeric()
a <- 0


if(is.na(as.numeric((x[1])))==FALSE){
   s[1-2*a] <- as.numeric(x[1])
} else{
   f <- get(x[1])
   s[1-2*a] <- f(s[1-2*a-2],s[1-2*a-1])
   s <- s[-c(1-2*a-1,1-2*a-2)]
   a <- a+1
} 

a
s

if(is.na(x[2])==FALSE){
   s[2-2*a] <- as.numeric(x[2])
} else{
   f <- get(x[2])
   s[2-2*a] <- f(s[2-2*a-2],s[2-2*a-1])
   s <- s[-c(2-2*a-1,2-2*a-2)]
   a <- a+1
}  

a
s

if(is.na(as.numeric((x[3])))==FALSE){
   s[3-2*a] <- as.numeric(x[3])
} else{
   f <- get(x[3])
   s[3-2*a] <- f(s[3-2*a-2],s[3-2*a-1])
   s <- s[-c(3-2*a-1,3-2*a-2)]
   a <- a+1
}  

a;s

if(is.na(as.numeric((x[4])))==FALSE){
   s[4-2*a] <- as.numeric(x[4])
} else{
   f <- get(x[3])
   s[3-2*a] <- f(s[3-2*a-2],s[3-2*a-1])
   s <- s[-c(3-2*a-1,3-2*a-2)]
   a <- a+1
}  
a;s

if(is.na(as.numeric((x[5])))==FALSE){
   s[3-2*a] <- as.numeric(x[3])
} else{
   f <- get(x[5])
   f
   s[5-2*a] <- f(s[5-2*a-2],s[5-2*a-1])
   s <- s[-c(5-2*a-1,5-2*a-2)]
   s
   a <- a+1
}  
a; s

if(is.na(as.numeric((x[6])))==FALSE){
   s[6-2*a] <- as.numeric(x[6])
} else{
   f <- get(x[5])
   f
   s[5-2*a] <- f(s[5-2*a-2],s[5-2*a-1])
   s <- s[-c(5-2*a-1,5-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[7])))==FALSE){
   s[7-2*a] <- as.numeric(x[7])
} else{
   f <- get(x[5])
   f
   s[5-2*a] <- f(s[5-2*a-2],s[5-2*a-1])
   s <- s[-c(5-2*a-1,5-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[8])))==FALSE){
   s[6-2*a] <- as.numeric(x[6])
} else{
   f <- get(x[8])
   f
   s[8-2*a] <- f(s[8-2*a-2],s[8-2*a-1])
   s <- s[-c(8-2*a-1,8-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[9])))==FALSE){
   s[9-2*a] <- as.numeric(x[9])
} else{
   f <- get(x[8])
   f
   s[8-2*a] <- f(s[8-2*a-2],s[8-2*a-1])
   s <- s[-c(8-2*a-1,8-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[10])))==FALSE){
   s[10-2*a] <- as.numeric(x[10])
} else{
   f <- get(x[8])
   f
   s[8-2*a] <- f(s[8-2*a-2],s[8-2*a-1])
   s <- s[-c(8-2*a-1,8-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[11])))==FALSE){
   s[6-2*a] <- as.numeric(x[6])
} else{
   f <- get(x[11])
   f
   s[11-2*a] <- f(s[11-2*a-2],s[11-2*a-1])
   s <- s[-c(11-2*a-1,11-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[12])))==FALSE){
   s[6-2*a] <- as.numeric(x[6])
} else{
   f <- get(x[12])
   f
   s[12-2*a] <- f(s[12-2*a-2],s[12-2*a-1])
   s <- s[-c(12-2*a-1,12-2*a-2)]
   s
   a <- a+1
}
a; s

if(is.na(as.numeric((x[13])))==FALSE){
   s[6-2*a] <- as.numeric(x[6])
} else{
   f <- get(x[13])
   f
   s[13-2*a] <- f(s[13-2*a-2],s[13-2*a-1])
   s <- s[-c(13-2*a-1,13-2*a-2)]
   s
   a <- a+1
}
a; s
