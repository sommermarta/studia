# 4.10

permutacje <- function(x){
   
   stopifnot(is.atomic(x))
   stopifnot(length(x)>1)
   #stopifnot(floor(n)==n)
   #stopifnot(n>=1)
   
   n <- length(x)
   tab <- matrix(0,nrow=factorial(n),ncol=n)
   tab[1,] <- sort(x)
   
   p <- sort(x)
   
   for(z in 2:factorial(n)){
      
      for(i in n:2){
         if(p[i-1] < p[i]) break
      }
      
      for(k in n:i){
         if(p[k]>p[i-1]){
            a <- p[i-1]
            p[i-1] <- p[k]
            p[k] <- a
            break
         }
      }
      
      p[i:n] <- sort(p[i:n]) 
      tab[z,] <- p
   }
   
   tab
   
}


permutacje(1:4)
permutacje(1:2)
permutacje(1:3)
permutacje(-3)
permutacje(3.5)
permutacje(NA)
permutacje(c())
permutacje(1:6)

x <- c("dfg",3, TRUE)
permutacje(x)

########################################################################
# 4.12

m <- matrix(c(7,4,8,9,6,1,3,5,2,2,6,5,4,8,3,9,7,1,9,3,1,5,7,2,6,4,8,6,9,3,
              1,4,5,8,2,7,1,7,4,6,2,8,5,3,9,5,8,2,7,3,9,4,1,6,3,1,6,2,9,4,
              7,8,5,8,2,7,3,5,6,1,9,4,4,5,9,8,1,7,2,6,3), ncol=9, 
            byrow=TRUE)

mm <- matrix(c(7,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
               9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
               1,6,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
             ncol=9, byrow=TRUE)


plansza_sudoku <- function(m){
   
   stopifnot(is.matrix(m))
   stopifnot(is.numeric(m))
   d <- dim(m)
   stopifnot(d[1]==d[2])
   m[is.na(m)] <- 0
   k <- sqrt(d[1])
   stopifnot(floor(k)==k)
   stopifnot(all(as.vector(floor(m)==m)==TRUE))
   
   if(!all(m<=d[1])) return(FALSE)
   if(!all(m>=0)) return(FALSE)
   
   v <- numeric(3*d[1])
   
   for(i in 1:d[1]){
      l1 <- length(which(m[i,]==0))
      if(l1==0) l1 <- 1
      v[i] <- length(unique(m[i,]))==d[1]-(l1-1)
      
      l2 <- length(which(m[,i]==0))
      if(l2==0) l2 <- 1
      v[d[1]+i] <- length(unique(m[,i]))==d[1]-(l2-1)
   }
   
   z <- matrix(0,ncol=k,nrow=k)
   s <- 0
   
   for(i in 0:(k-1)){
      for(r in 0:(k-1)){
         z <- m[(i*k+1):(i*k+k),(r*k+1):(r*k+k)]
         a <- as.vector(z)
         ll <- length(which(a==0))
         if(ll==0) ll <- 1
         
         v[2*d[1]+r+1+k*s] <- length(unique(a))==d[1]-ll+1
      }
      s <- s+1
   }
   
   if(all(as.logical(v))==TRUE) TRUE else FALSE
   
}

plansza_sudoku(m)
plansza_sudoku(mm)

mmm <- matrix(c(7,4,8,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
               9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
               1,6,2.8,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
             ncol=9, byrow=TRUE)
plansza_sudoku(mmm)

m3 <- matrix(c(7,4,2,9,NA,1,NA,5,2,2,6,NA,4,8,3,9,NA,1,9,3,1,5,7,2,6,4,8,6,
                9,3,1,4,5,8,NA,7,NA,7,4,NA,2,8,5,3,9,5,NA,2,7,3,9,4,1,6,3,
                1,6,2,9,4,7,8,5,8,2,7,NA,5,6,NA,9,4,4,5,9,8,1,7,2,6,3), 
              ncol=9, byrow=TRUE)
plansza_sudoku(m3)

##########################################################################
# 4.9

niemoralny <- function(m){
   
   stopifnot(is.matrix(m))
   stopifnot(is.numeric(m))
   dd <- dim(m)
   stopifnot(dd[1]==dd[2])
   stopifnot(is.element(as.vector(m),c(0,1)))
   
   d <- dd[1]
   
   for(j in 1:d){
      if(sum(m[,j])>1){
         w <- which(m[,j]==1)
         if(all(as.vector(m[w,w])==0) & all(as.vector(m[j,w])==0)){
            ost <- TRUE
            return(TRUE)
         } 
      }
   }
   
   FALSE
   
}

m <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,0), ncol=4)
m
m[diag(m)] <- 1
m
niemoralny(m)

m <- matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,0),ncol=5)
niemoralny(m)

m <- matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0,0,1,1,0),ncol=5)
niemoralny(m)

m <- matrix(c(0,0,0,2,1,0,0,0,1,0,0,1,0,0,0,0), ncol=4)
niemoralny(m)

niemoralny(c())

niemoralny(matrix())

m <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0), ncol=4)
niemoralny(m)


#########################################################################
#4.14

domk_przech <- function(m){
   
   stopifnot(is.matrix(m))
   stopifnot(is.numeric(m))
   dd <- dim(m)
   stopifnot(dd[1]==dd[2])
   stopifnot(is.element(as.vector(m),c(0,1)))
   
   n <- dd[1]
   mm <- m
   
   for(i in 1:n){
      for(j in 1:n){
         if(m[i,j]==1){
            w <- which(m[j,]==1)
            if(length(w)==0) next
            mm[i,w] <- 1
         }
      }
   }
   
   mm
   
}


m <- matrix(c(0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1),ncol=4)
domk_przech(m)

m <- matrix(c(0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)

m <- matrix(c(0,4,1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)

m <- matrix(c(0,"k",1,0,1,0,1,0,0,0,0,0,1,1,0,1),ncol=4)
domk_przech(m)

m <- matrix(c(0,1,1,0,1,0,1,0,0,0,0,0),ncol=4)
domk_przech(m)

m <- matrix(c(1,0,1,0,1,0,0,1,0),ncol=3)
domk_przech(m)

m <- matrix(c(1,1,1,1,1,1,1,1,1),ncol=3)
domk_przech(m)

m <- matrix(c(0,1,1,1,0,1,1,1,0),ncol=3)
domk_przech(m)

##########################################################################
# 4.15


klabs <- function(m){
   
   stopifnot(is.matrix(m))
   stopifnot(is.numeric(m))
   
   dd <- dim(m)
   
   stopifnot(dd[1]==dd[2])
   stopifnot(is.element(as.vector(m),c(0,1)))
   if(!all((diag(m)==1))) stop("Relacja nie jest zwrotna!")         
   if(any(as.vector(t(m)==m)==FALSE)) stop("Relacja nie jest symetryczna!")            
   
   n <- dd[1]
   
   for(i in 1:n){             
      for(j in 1:n){
         if(m[i,j]==1){
            w <- which(m[j,]==1)
            if(length(w)==0) next
            if(any(as.vector(m[i,w])!=1)) 
               stop("Relacja nie jest przechodnia!")
         }
      }
   }

   ka <- vector("list",n)
   
   for(i in 1:n){
      w <- which(m[i,]==1)
      ka[[i]] <- w
   }
   
   ka
   
}

m <- matrix(c(0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,1),ncol=4)
klabs(m)

m <- matrix(c(1,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1),ncol=4)
klabs(m)

m <- matrix(c(1,0,0,1,0,1,0,1,0,0,1,0,1,1,0,1),ncol=4)
klabs(m)

m <- matrix(c(1,0,0,0,0,1,1,1,0,1,1,1,0,1,1,1),ncol=4)
klabs(m)

m <- matrix(c(1,1,1,1,1,1,1,1,1),ncol=3)
klabs(m)

m <- matrix(c(1,0,0,0,0,1,1,1,0,1,1,1,0,1,1,1),ncol=4)
klabs(m)

m <- matrix(c(1,0,1,0,0,1,0,0,0,0,1,1,0,0,1,1),ncol=4)
klabs(m)

klabs(matrix())

klabs(diag(4))

##########################################################################
# 4.16


relrown <- function(l){
   
   stopifnot(is.list(l))
   stopifnot(is.numeric(unlist(l)))
   
   ll <- length(l)
   m <- matrix(0,ncol=ll,nrow=ll)
   
   for(i in 1:ll){
      m[i,l[[i]]] <- 1
   }
   
   m
   
}


l <- list(1,2:4,2:4,2:4)
relrown(l)

l <- list(1,2,3:4,3:4)
relrown(l)

l <- list(1,2,3,4)
relrown(l)

#########################################################################
# 4.12

naiwne_sudoku <- function(m){
   
   w <- which(is.na(m))
   m[w] <- 0
   
   n <- dim(m)[1]
   nn <- sqrt(n)
   e <- 1:n
   
   f <- function(i){
      a <- intersect(s[[i]],k[[i]])
      intersect(a,z[[i]])
   }
   
   ff <- function(i){
      length(wsp[[i]])==1
   }
   
   repeat{
      
      k <- vector("list",n*n)
      s <- vector("list",n*n)
      z <- vector("list",n*n)
      
      for(j in 1:n){
         for(i in 1:n){
            if(m[i,j]==0){
               
               if(all(m[i,]==0)){
                  k[[n*(j-1)+i]] <- e
               } else{
                  k[[n*(j-1)+i]] <- e[-m[i,]]
               }
               
               if(all(m[,j]==0)){
                  s[[n*(j-1)+i]] <- e 
               } else{
                  s[[n*(j-1)+i]] <- e[-m[,j]]
               }
               d <- (i-1) %/% nn
               dd <- (j-1) %/% nn
               
               if(all(as.vector(m[(d*nn+1):(d*nn+nn),
                                  (dd*nn+1):(dd*nn+nn)])==0)){
                  z[[n*(j-1)+i]] <- e
               } else{
                  z[[n*(j-1)+i]] <- e[-as.vector(m[(d*nn+1):(d*nn+nn),
                                                   (dd*nn+1):(dd*nn+nn)])]
               }            
            } 
         }
      }
      
      wsp <- lapply(1:(n*n),f)
      
      if(!any(unlist(lapply(1:(n*n),ff)))) break
      
      for(i in 1:length(wsp)){
         if(length(wsp[[i]])==1) m[i] <- wsp[[i]]
      }
      
   }
   
   m[which(as.vector(m)==0)] <- NA
   m
   
}



m <- matrix(c(3,0,0,0,0,2,0,1,0,4,0,0,0,0,0,2),ncol=4)
plansza_sudoku(m)
naiwne_sudoku(m)


m <- matrix(c(3,NA,NA,1,2,NA,NA,NA,NA),ncol=3)
naiwne_sudoku(m)

m <- matrix(c(0,0,0,0,5,3,0,0,0,9,0,0,1,6,8,2,0,0,4,0,0,0,0,0,8,1,5,6,
              0,2,0,0,0,3,0,0,0,8,4,0,9,6,0,2,0,0,0,0,0,1,5,0,0,8,8,0,0,
              0,0,0,1,9,3,2,9,0,0,0,0,0,0,7,7,3,6,0,0,0,5,0,0),ncol=9)
plansza_sudoku(m)
naiwne_sudoku(m)


##########################################################################
# 4.13









