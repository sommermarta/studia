# 5.8

slowotwor <- function(x){
   
   stopifnot(is.numeric(x))
   stopifnot(length(x)>0)
   stopifnot(x<1000000000)
   stopifnot(x>-1000000000)

   n <- length(x)
   w <- character(n)
   
   for(j in 1:n){
      
      a <- as.list(unlist(stri_enc_toutf32(x[j])))
      
      if(length(a)==1 & a[[1]]==48){
         w[j] <- "zero" 
         break
      }
      
      if(a[1]==45){
         a <- a[-1]
         min <- "minus"
      } else min <- ""
      
      aa <- unlist(stri_enc_fromutf32(a))
      xx <- as.numeric(aa)
      lxx <- length(xx)
      
      rx <- rev(xx)
      s <- 0
      ce <- ceiling(lxx/3)
      l <- vector("list",ce)
      
      for(i in 1:ce){
         l[[ce-i+1]] <- rx[(1+s*3):(3+s*3)]
         s <- s+1
      }
      
      
      ll <- lapply(l,rev)
      
      f <- function(i){
         nnn <- is.na(ll[[i]])
         if(all(nnn==FALSE)==FALSE){
            ll[[i]][nnn] <- 0 
            ll[[i]] 
         } else ll[[i]]
         
      } 
      
      t <- lapply(1:ce,f)
      
      a <- c("jeden","dwa","trzy","cztery","piêæ","szeœæ","siedem","osiem",
             "dziewiêæ")
      b <- c("jedenaœcie","dwanaœcie","trzynaœcie","czternaœcie","piêtnaœcie",
             "szesnaœcie","siedemnaœcie","osiemnaœcie","dziewiêtnaœcie")
      c <- c("dziesiêæ","dwadzieœcia","trzydzieœci","czterdzieœci","piêædziesi¹t",
             "szeœædziesi¹t","siedemdziesi¹t","osiemdziesi¹t","dziewiêædziesi¹t")
      d <- c("sto","dwieœcie",stri_paste(a[3:4],"sta"),stri_paste(a[5:9],"set"))
      
      cop <- list(a,b,c,d)
      
      q <- function(i){
         
         w1 <- if(t[[i]][2]==0){
            cop[[1]][t[[i]][3]]
         } else{
            if(t[[i]][3]==0){
               cop[[3]][t[[i]][2]]
            } else{
               if(t[[i]][2]==1){
                  cop[[2]][t[[i]][3]]
               } else{
                  stri_paste(c(cop[[3]][t[[i]][2]],cop[[1]][t[[i]][3]]),collapse=" ")
               }
            }
         }
         
         
         w2 <- if(t[[i]][1]!=0) cop[[4]][t[[i]][1]]
         
         stri_paste(c(w2,w1),collapse=" ")
         
      }
      
      z <- lapply(1:ce,q)
      
      tys <- c("tysi¹c","tysi¹ce","tysiêcy")
      mil <- c("milion","miliony","milionów")
      
      o <- function(i){
         k <- t[[i]]
         k[1]*100+k[2]*10+k[3]
      }
      
      licz <- lapply(1:ce,o)
      
      hhh <- function(lt){
         
         if(lt==1) return(c(""))
         
         if(lt==2){
            wartys <- if(licz[[lt-1]]==1) tys[1] else if(licz[[lt-1]]<=4) tys[2] else if(licz[[lt-1]]<=21) tys[3] else if(ll[[lt-1]][[3]]<=4 & ll[[lt-1]][[3]]>=2) tys[2] else tys[3]
            return(c(wartys,""))
         }
         
         if(lt==3){
            wartys <- if(licz[[lt-1]]==1) tys[1] else if(licz[[lt-1]]<=4) tys[2] else if(licz[[lt-1]]<=21) tys[3] else if(ll[[lt-1]][[3]]<=4 & ll[[lt-1]][[3]]>=2) tys[2] else tys[3]
            warmil <- if(licz[[lt-2]]==1) mil[1] else if(licz[[lt-2]]<=4) mil[2] else if(licz[[lt-2]]<=21) mil[3] else if(ll[[lt-2]][[3]]<=4 & ll[[lt-2]][[3]]>=2) mil[2] else mil[3]
            return(c(warmil,wartys,""))   
         }
         
      }
      
      pam <- as.list(hhh(ce))
      
      e <- function(i){
         stri_paste(c(z[[i]],pam[[i]]),collapse=" ")
      }
      
      oo <- lapply(1:ce,e)
      
      w[j] <- stri_trim_both(stri_paste(c(min,unlist(oo)),collapse=" "))
      
   }
   
   w
   
}


slowotwor(c(45,1,-980,111,0))
slowotwor(144359)
slowotwor(4348153)
slowotwor(5523456000)
slowotwor(302)

###########################################################################

# 5.1

genpwd <- function(n=1,k=8,dodatkowe=c("_","-")){
   
   stopifnot(is.numeric(n))
   stopifnot(is.numeric(k))
   stopifnot(is.character(dodatkowe))
   stopifnot(length(n)==1)
   stopifnot(length(k)==1)
   stopifnot(length(dodatkowe)>=1)
   stopifnot(floor(n)==n)
   stopifnot(floor(k)==k)
   stopifnot(k>=4)
   
   f <- function(i){
      numeric(k)
   }
   
   ost <- lapply(1:n,f)
   
   z <- unlist(stri_enc_toutf32(dodatkowe))
   d <- 65:90
   m <- 97:122
   l <- 48:57
   
   ff <- function(i){
      c(sample(d,1),sample(m,1),sample(l,1),sample(z,1))
   }
   
   co <- lapply(1:n,ff)
   
   fff <- function(i){
      sample(1:k,4)
   }
   
   gdzie <- lapply(1:n,fff)
   
   g <- function(i){
      ost[[i]][gdzie[[i]]] <- co[[i]]
      ost[[i]]
   }
   
   to <- lapply(1:n,g)
   
   gg <- function(i){
      sample(c(d,m,l,z),k-4,replace=TRUE)
   }
   
   r <- lapply(1:n,gg)
   
   ggg <- function(i){
      to[[i]][-gdzie[[i]]] <- r[[i]]
      to[[i]]
   }
   
   toto <- lapply(1:n,ggg)
   
   h <- function(i){
      stri_enc_fromutf32(toto[[i]])
   }
   
   unlist(lapply(1:n,h))
   
}


genpwd()
genpwd(4,5)
genpwd(3,7,c("%","(","#","_"))
genpwd(3.2)
genpwd(2,4.4)
genpwd(2,1)
genpwd(5,4,c(";","_"))


###########################################################################
# 5.7

nbsp <- function(x,nbspchar){
   
   stopifnot(is.character(x))
   stopifnot(is.character(nbspchar))
   stopifnot(length(x)>0)
   stopifnot(length(nbspchar)==1)
   
   n <- length(x)  
   bezspac <- character(n)
   ostat <- character(n)
   
   f <- function(i){
      if(dlug[[i]]<=2) l[[i]] <- nbspchar
      l[[i]]
   }
   
   ff <- function(i){
      stri_paste(aaa[[i]],znaki[[i]])
   }
   
   for(k in 1:n){
      
      xx <- stri_trim_both(x[k])
      ut <- unlist(stri_enc_toutf32(xx))
      lut <- length(ut)
      p <- logical(lut-1)
      
      for(i in 1:(lut-1)){
         if(ut[i]==32 & ut[i+1]==32) p[i+1] <- TRUE
      }
      
      w <- which(p)
      uw <- ut[-w]
      
      bezspac[k] <- unlist(stri_enc_fromutf32(uw))
      
      aaa <- as.list(unlist(stri_split_fixed(bezspac[k]," ")))
      dlug <- lapply(aaa,stri_length)
      
      l <- as.list(rep(" ",length(dlug)))
      
      znaki <- lapply(1:length(dlug),f)
      
      y <- lapply(1:(length(dlug)-1),ff)
      y[[length(dlug)]] <- aaa[[length(dlug)]]
      uy <- unlist(y)
      
      ostat[k] <- stri_paste(uy,collapse="")
      
   }
   
   ostat
   
}

x <- c("    Na zielonym          œwietle     hop-hop, a na czerwonym prr.   ",
       "jestê     zielon¹ traw    k¹ j¹!!!!  ")
nb <- "~"

nbsp(x,nb)

x <- c("    Na zielonym    œwietle     hop-hop, a na czerwonym prr.   ",
       "jestê     zielon¹ traw    k¹ j¹!!!!      w")
nb <- "^"

nbsp(x,nb)



##########################################################################

# 5.3

sprNawiasy <- function(tekst,nawiasy=matrix(c("(","{","[",")","}","]"),ncol=2)){
   
   stopifnot(is.character(tekst))
   stopifnot(length(tekst)>0)
   stopifnot(all(stri_length(tekst)>0))
   stopifnot(is.character(nawiasy))
   stopifnot(is.matrix(nawiasy))
   stopifnot(ncol(nawiasy)==2)
   stopifnot(all(stri_length(nawiasy)==1))
   
   n <- length(tekst)
   
   u <- unlist(stri_enc_toutf32(nawiasy))
   dim(u) <- dim(nawiasy)
   
   y <- numeric(n)
   
   for(k in 1:n){
      
      t <- unlist(stri_enc_toutf32(tekst[k]))
      lt <- length(t)
      
      o <- numeric(nrow(nawiasy))
      z <- numeric(nrow(nawiasy))
      
      for(i in 1:lt){
         if(!is.element(t[i],u)) next else{
            if(is.element(t[i],u[,1])){
               w <- which(t[i]==u[,1]) 
               o[w] <- o[w] + 1
            } else{
               w <- which(t[i]==u[,2]) 
               z[w] <- z[w] + 1
            }
         }
         
         if(any(z > o)){
            y[k] <- i
            break
         } else if(all(o==z)){
            y[k] <- 0 
         } else y[k] <- i
         
      }      
   }
   
   y
   
}


sprNawiasy("(())")
sprNawiasy("[(())]")
sprNawiasy("aaa(())")
sprNawiasy("(())[sasa")
sprNawiasy("(())]ss")
sprNawiasy("(())[ss]")
sprNawiasy("a({22vaf{f}ffaga}")
sprNawiasy("[(a {((b)(c))}(d)e)]")
sprNawiasy(c("[[]{}ddd(()","{[(a ((b)(c))(d)e)]}"))
sprNawiasy("(")
sprNawiasy("a((b)a)",matrix(c("a","b"),ncol=2))

sprNawiasy("",matrix(c("a","b"),ncol=2))
sprNawiasy("aabbabbbbbb",matrix(c("a","b"),ncol=2))
sprNawiasy("aabbabbbbbb",matrix(c("a","bb"),ncol=2))


##########################################################################

# 5.2

uncomment <- function(tekst,znaczniki){
   
   n <- length(tekst)
   b <- character(n)
   
   for(k in 1:n){
      
      tek <- tekst[k]
      
      repeat{
         
         po <- stri_locate_first_fixed(tek,znaczniki[,1])
         
         napo <- na.omit(po)
         if(length(napo)==0) break
         
         w <- which(po==min(napo))
         popo <- po[w,]
         
         z <- znaczniki[w,2]
         koko <- stri_locate_first_fixed(tek,z)
         
         mi <- min(popo)
         ma <- max(koko)
         
         teu <- unlist(stri_enc_toutf32(tek))
         
         if(!is.na(ma)) ty <- teu[-(mi:ma)] else ty <- teu[-(mi:length(teu))]
         
         tek <- stri_enc_fromutf32(ty)
      }
      
      b[k] <- tek
   }
   
   b
   
}

t <- c("1 ///* tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg",
       "tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg")
z <- matrix(c("/*","//","*/",NA),ncol=2)

uncomment(t,z)


