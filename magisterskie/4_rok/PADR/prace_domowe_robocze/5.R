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
   
   lapply(1:n,h)
      
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

library("stringi")

nawiasy <- matrix(c("(","{","[",")","}","]"),ncol=2)
nawiasy

tekst <- "({{}}"
tekst

n <- length(tekst)
n
nn <- nrow(nawiasy)
nn

f <- function(i){
   stri_locate_all_fixed(tekst,nawiasy[i,])
}

l <- lapply(1:nn,f)
l

ff <- function(i){
   l[[i]][[1]][,1]
}

o <- lapply(1:nn,ff)
o

fff <- function(i){
   l[[i]][[2]][,1]
}

z <- lapply(1:nn,fff)
z

lo <- lapply(o,length)
lz <- lapply(z,length)

lo;lz

#

g <- function(i){
   c(o[[i]],z[[i]])
}

gdzie <- lapply(1:nn,g)
gdzie

gg <- function(i){
   rep(c(1,2),each=lo[[i]])
}

co <- lapply(1:nn,gg)
co

ggg <- function(i){
   co[[i]][order(gdzie[[i]])]
}

coco <- lapply(1:nn,ggg)
coco

ss <- lapply(gdzie,sort)
ss

h <- function(i){
   
   llo <- 0
   llz <- 0
   
   for(k in 1:(2*lo[[i]])){
      
      if(coco[[i]][k]==1) llo <- llo + 1 else llz <- llz + 1
      if(llz > llo) break
   }
   
   c(ss[[i]][k],k)
   
}


pop <- lapply(1:nn,h)
pop

hh <- function(i){
   if(pop[[i]][2]==2*lo[[i]]) 0 else pop[[i]][1] 
} 
 
sss <- sapply(1:nn,hh)
sss

c <- all(sapply(1:nn,function(i){lo[[i]]==lz[[i]]}))
war <- min(sss[-which(sss==0)])

if(all(sss==0) & c) 0 else if(war==0 & !c) stri_length(tekst) else war


#   
   
   
   
   
   
   
   
   
   
























for(i in 1:length(o)){
   
   if(coco[i]==1) llo <- llo + 1 else llz <- llz + 1
   
   if(llz > llo) break
}

i






sprNawiasy(tekst,nawiasy=matrix(c("(","{","[",")","}","]"),ncol=2)){
   
   f <- function(i){
      stri_locate_all_fixed(t,nawiasy[i,])
   }
   
   o <- n1[[1]][,1]
   z <- n1[[2]][,1]
   
   length(o)==length(z)
   
   gdzie <- c(o,z)
   co <- rep(c(1,2),each=length(o))
   coco <- co[order(gdzie)]
   
   lo <- 0
   lz <- 0
   
   for(i in 1:length(o)){
      
      if(coco[i]==1) lo <- lo + 1 else lz <- lz + 1
      
      if(lz>lo) break
   }
   
}

#######################################################################



#########################################################################


sjp <- read.table("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\PADR\\sjp2.txt")
head(sjp)

readLines(sjp2.txt)



litery <- c("a","¹","b","c","æ","d","e","ê","f","g","h","i","j","k","l","³",
            "m","n","ñ","o","ó","p","r","s","œ","t","u","w","y","z","Ÿ","¿")

liczby <- c(1,5,3,2,6,2,1,5,5,3,3,1,3,2,2,3,2,1,7,1,5,2,1,1,5,2,3,1,2,1,9,5)

llit <- length(litery)
llic <- length(liczby)

sjp <- readLines("C:\\Users\\Marta\\Desktop\\Marta\\studia\\rok4\\PADR\\a.txt",
                 encoding="utf8")
sjp[24]


x <- c("j","r","a","t","e","p")
n <- 3
lx <- length(x)

permutacje(x)
install.packages("permute")
library("permute")
a <- matrix(ncol=lx,nrow=factorial(lx))
a[2:factorial(lx),] <- allPerms(1:lx)
a[1,] <- 1:lx
a

m <- matrix(ncol=lx,nrow=factorial(lx))
m
for(i in 1:factorial(lx)){
   m[i,] <- x[a[i,]]
}
m

library("stringi")

stri_paste(m[1,], collapse="")




i <- 1

##########################################################################

nawiasy <- matrix(c("(","{","[",")","}","]"),ncol=2)
nawiasy

tekst <- "a({22vaf{f}ffaga}"
tekst

tekst <- "[((])))[]))"

u <- unlist(stri_enc_toutf32(nawiasy))
dim(u) <- dim(nawiasy)
u

t <- unlist(stri_enc_toutf32(tekst))
t
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
      y <- i
      break
   }  else if(all(o==z)){
      y <- 0 
      } else y <- i
   
}

y

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

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

# 5.2

tekst <- "1 /* tup tup /* tup // aa */ 2" 
tekst
znaczniki <- matrix(c("/*","//","*/",NA),ncol=2)
znaczniki

znaczniki[1,]
a <- unlist(stri_locate_all_fixed(tekst,znaczniki[1,1]))[1:2]
b <- unlist(stri_locate_all_fixed(tekst,znaczniki[1,2]))[1:1]

a;b
 
c <- unlist(stri_locate_all_fixed(tekst,znaczniki[2,1]))[1:1]
d <- unlist(stri_locate_all_fixed(tekst,znaczniki[2,2]))[1:1]

c;d

ss <- sort(c(a,b,c,d))
lss <- length(ss)

o <- stri_sub(tekst, ss[1],ss[1]+1)
o
oo <- stri_enc_toutf32(o)
znaczniki
z <- znaczniki[which(is.element(znaczniki,o))+nrow(znaczniki)]
z

for(i in 2:lss){

   stri_sub(tekst,)
   
}

i <- 1
####################################################################


tekst <- c("1 ///* tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg",
           "tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg")
tekst
znaczniki <- matrix(c("/*","//","*/",NA),ncol=2)
znaczniki

n <- length(tekst)
b <- character(n)


for(k in 1:n){
   
   tek <- tekst[k]
   
   repeat{
      
      po <- stri_locate_first_fixed(tek,znaczniki[,1])
      po
      
      napo <- na.omit(po)
      if(length(napo)==0) break
      
      w <- which(po==min(napo))
      w
      #if(length(w)==0) break
      popo <- po[w,]
      popo
      z <- znaczniki[w,2]
      z
      
      koko <- stri_locate_first_fixed(tek,z)
      koko
      
      mi <- min(popo)
      mi
      ma <- max(koko)
      ma 
      
      teu <- unlist(stri_enc_toutf32(tek))
      teu
      if(!is.na(ma)) ty <- teu[-(mi:ma)] else ty <- teu[-(mi:length(teu))]
      
      tek <- stri_enc_fromutf32(ty)
      tek
      
   }
   
   b[k] <- tek
   
   
}


b


tekst <- c("hgahjagh7367#jshgdjkadhk","#jjjjjj")
znaczniki <- matrix(c("#",NA),ncol=2)

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
         w <- w[1]
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

b[2]

















repeat{
   
   po <- stri_locate_first_fixed(tekst,znaczniki[,1])
   po
   
   napo <- na.omit(po)
   if(length(napo)==0) break
   
   w <- which(po==min(napo))
   w
   #if(length(w)==0) break
   popo <- po[w,]
   popo
   z <- znaczniki[w,2]
   z
   
   koko <- stri_locate_first_fixed(tekst,z)
   koko
   
   mi <- min(popo)
   mi
   ma <- max(koko)
   ma 
   
   teu <- unlist(stri_enc_toutf32(tekst))
   teu
   if(!is.na(ma)) ty <- teu[-(mi:ma)] else ty <- teu[-(mi:length(teu))]
   
   tekst <- stri_enc_fromutf32(ty)
   tekst
   
}

tekst

po <- stri_locate_first_fixed(tekst,znaczniki[,1])
po

w <- which(po==min(po))
w
popo <- po[w,]
popo
z <- znaczniki[w,2]

koko <- stri_locate_first_fixed(tekst,z)

mi <- min(popo)
ma <- max(koko)

teu <- unlist(stri_enc_toutf32(tekst))
teu
ty <- teu[-(mi:ma)]

tekst <- stri_enc_fromutf32(ty)
tekst


