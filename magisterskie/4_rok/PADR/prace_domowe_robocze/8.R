n <- 50
y <- sort(rnorm(n))
x <- 1:n

plot.new()
plot.window(c(-n,n),c(0,max(y)))
points(x,y)

##################################################3

elipsa <- function(a=1,b=1,n=1000,x0=0,y0=0,...){
   
   teta <- seq(0, 2*pi, length=n+1)[-1]
   x <- x0 + a*cos(teta)
   y <- y0 + b*sin(teta)
   
   polygon(x, y, asp=1, ...)
   
}

x <- sample(1:100,80)
y <- sample(1:100,50)


li <- length(intersect(x,y))

lx <- length(x)
ly <- length(y)

if(lx<ly){
   t <- x
   x <- y
   y <- t
}

# x- wiekszy

lx <- length(x)
ly <- length(y)

ile <- lx/pi
c <- round(sqrt(ile),1)
d <- ile/c

a1 <- max(c,d)
b1 <- min(c,d)

ile <- ly/pi
c <- round(sqrt(ile),1)
d <- ile/c

a2 <- max(c,d)
b2 <- min(c,d)

a1;b1;a2;b2

szer <- ceiling(2*a1+2*a2+1)/2
wys <- ceiling(max(b1,b2)+1)

to <- max(wys,szer)

plot.new()
plot.window(c(-to,to),c(-to,to))

points(-to,to)
points(-to,-to)
points(to,-to)
points(to,to)
points(0,0)


if(li==0){
   elipsa(x0=-to+a1,a=a1,b=b1,col="blue")
   elipsa(x0=to-a2,a=a2,b=b2,col="red")
   
   text(-to+a1,b2+1,stri_paste(c("A",lx),collapse=" - "),pos=3)
   lines(c(-to+a1,-to+a1),c(b2+1,0))
   
   text(to-a2,-b1-1,stri_paste(c("B",ly),collapse=" - "),pos=1)
   lines(c(to-a2,to-a2),c(-b1-1,0))
   
} 

if(li==ly){
   if(lx==ly){
      
      elipsa(a=a1,b=b1,col="blue")
      text(0,b2+1,stri_paste(c("A = B = A and B",lx),collapse=" - "),pos=3)
      lines(c(0,0),c(0,b2+1))
      
   } else{
      elipsa(a=a1,b=b1,col="blue")
      elipsa(a=a2,b=b2,col="red")
      
      text(0,b1+(to-b1)/2,stri_paste(c("A",lx),collapse=" - "),pos=3)
      lines(c(0,0),c(b1+(to-b1)/2,b2+(b1-b2)/2))
      
      
      text(0,-b1-(to-b1)/2,stri_paste(c("B = A and B",ly),collapse=" - "),pos=1)
      lines(c(0,0),c(-b1-(to-b1)/2,0))
   } 
}


if(li>0 & li != ly){
   u <- (-1)*(a1-(li/ly)*2*a2)
   elipsa(x0=u,a=a1,b=b1,col="#00ff00d5")
   elipsa(x0=a2,a=a2,b=b2,col="#ff0000d5")
   
   text(-a1-(to-a1)/2,b2+(to-b2)/2,stri_paste(c("A",lx),collapse=" - "),pos=3)
   lines(c(-a1-(to-a1)/2,2*a2-2*a1-(2*a1-2*a2)/2),
         c(b2+(to-b2)/2,0))
   
   points(-a1-(to-a1)/2,b2+(to-b2)/2)
   points(-(2*a1-2*a2)/2,0,col="red")
   
   text(2*a2+(to-2*a2)/2,-b2-(to-b2)/2,stri_paste(c("B",ly),collapse=" - "),pos=1)
   lines(c(2*a2+(to-2*a2)/2,(li/ly)*2*a2+(2*a2-(li/ly)*2*a2)/2),
         c(-b2-(to-b2)/2,0))
   
   text((li/ly)*a2,b2+(to-b2)/2,stri_paste(c("A and B",li),collapse=" - "),pos=3)
   lines(c((li/ly)*a2,(li/ly)*a2),
         c(b2+(to-b2)/2,0))
   
}



venn <- function(x,y,colx="#00ff00d5",coly="#ff0000d5"){
   
   elipsa <- function(a=1,b=1,n=1000,x0=0,y0=0,...){      
      teta <- seq(0, 2*pi, length=n+1)[-1]
      x <- x0 + a*cos(teta)
      y <- y0 + b*sin(teta)
      
      polygon(x, y, asp=1, ...)     
   }
     
   li <- length(intersect(x,y))  
   lx <- length(x)
   ly <- length(y)
   
   if(lx<ly){
      t <- x
      x <- y
      y <- t
      m <- colx
      colx <- coly
      coly <- m
   }
   
   lx <- length(x)
   ly <- length(y)
   
   ile <- lx/pi
   c <- round(sqrt(ile),1)
   d <- ile/c
   
   a1 <- max(c,d)
   b1 <- min(c,d)
   
   ile <- ly/pi
   c <- round(sqrt(ile),1)
   d <- ile/c
   
   a2 <- max(c,d)
   b2 <- min(c,d)
   
   szer <- ceiling(2*a1+2*a2+1)/2
   wys <- ceiling(max(b1,b2)+1)
   
   to <- max(wys,szer)
   
   plot.new()
   plot.window(c(-to,to),c(-to,to))
   
   if(li==0){
      elipsa(x0=-to+a1,a=a1,b=b1,col=colx)
      elipsa(x0=to-a2,a=a2,b=b2,col=coly)
      
      text(-to+a1,b2+1,stri_paste(c("A",lx),collapse=" - "),pos=3)
      lines(c(-to+a1,-to+a1),c(b2+1,0))
      
      text(to-a2,-b1-1,stri_paste(c("B",ly),collapse=" - "),pos=1)
      lines(c(to-a2,to-a2),c(-b1-1,0))
      
   } 
   
   if(li==ly){
      if(lx==ly){
         
         elipsa(a=a1,b=b1,col=colx)
         text(0,b2+1,stri_paste(c("A = B = A and B",lx),collapse=" - "),pos=3)
         lines(c(0,0),c(0,b2+1))
         
      } else{
         elipsa(a=a1,b=b1,col=colx)
         elipsa(a=a2,b=b2,col=coly)
         
         text(0,b1+(to-b1)/2,stri_paste(c("A",lx),collapse=" - "),pos=3)
         lines(c(0,0),c(b1+(to-b1)/2,b2+(b1-b2)/2))
         
         text(0,-b1-(to-b1)/2,stri_paste(c("B = A and B",ly),collapse=" - "),pos=1)
         lines(c(0,0),c(-b1-(to-b1)/2,0))
      } 
   }
   
   if(li>0 & li != ly){
      u <- (-1)*(a1-(li/ly)*2*a2)
      elipsa(x0=u,a=a1,b=b1,col=colx)
      elipsa(x0=a2,a=a2,b=b2,col=coly)
      
      text(-a1-(to-a1)/2,b2+(to-b2)/2,stri_paste(c("A",lx),collapse=" - "),pos=3)
      lines(c(-a1-(to-a1)/2,-(2*a1-(li/ly)*2*a2)/2),
            c(b2+(to-b2)/2,0))
      
      text((li/ly)*2*a2+(2*a2-(li/ly)*2*a2)/2,-b2-(to-b2)/2,stri_paste(c("B",ly),collapse=" - "),pos=1)
      lines(c((li/ly)*2*a2+(2*a2-(li/ly)*2*a2)/2,(li/ly)*2*a2+(2*a2-(li/ly)*2*a2)/2),
            c(-b2-(to-b2)/2,0))
      
      text((li/ly)*a2,b2+(to-b2)/2,stri_paste(c("A and B",li),collapse=" - "),pos=3)
      lines(c((li/ly)*a2,(li/ly)*a2),
            c(b2+(to-b2)/2,0))
      
   }
   
}

x <- sample(1:100,80)
y <- sample(1:100,10)
length(intersect(x,y))
venn(x,y)

x <- sample(1:100,70)
y <- sample(1:100,80)
length(intersect(x,y))
venn(x,y)

x <- sample(1:100,10)
y <- sample(1:100,10)
length(intersect(x,y))
venn(x,y)

x <- sample(1:100,90)
y <- sample(1:100,90)
length(intersect(x,y))
venn(x,y)

x <- sample(1:100,100)
y <- sample(1:100,50)
length(intersect(x,y))
venn(x,y)

x <- sample(1:100,1)
y <- sample(1:100,1)
length(intersect(x,y))
venn(x,y)

x <- sample(1:50,50)
y <- sample(1:50,50)
length(intersect(x,y))
venn(x,y)


#####################################################################

zwierz <- function(){
   
   elipsa <- function(a=2,b=3,n=1000,x0=0,y0=0,...){
      
      teta <- seq(0, 2*pi, length=n+1)[-1]
      x <- x0 + a*cos(teta)
      y <- y0 + b*sin(teta)
      
      polygon(x, y, asp=1, ...)
      
   }
   
   
   circle <- function(x0, y0, r, n=100, ...) {
      
      theta <- seq(0, 2*pi, length=n+1)[-1]
      x <- x0+r*cos(theta)
      y <- y0+r*sin(theta)
      polygon(x, y, ...)
      
   }
   
   plot.new()
   plot.window(c(0,2),c(0,2))
   
   theta <- seq(-pi/3, pi+pi/3, length=70)
   x <- 1+cos(theta)
   y <- 1+sin(theta)
   
   for(i in 1:length(theta)){
      lines(c(1,x[i]),
            c(1,y[i]),col="brown4")
   }
   
   
   xx <- x[seq(from=1,to=70,by=3)]
   yy <- y[seq(from=1,to=70,by=3)]
   
   palette(rainbow(n=70))
   
   for(i in 1:length(xx)){
      circle(xx[i],yy[i],r=1/17,col=sample(1:70,1))
      circle(xx[i],yy[i],r=1/120,col="black")
   }
   
   
   x <- 1+2/3*cos(theta)
   y <- 1+2/3*sin(theta)
   
   xx <- x[seq(from=2,to=70,by=3)]
   yy <- y[seq(from=2,to=70,by=3)]
   
   for(i in 1:length(xx)){
      circle(xx[i],yy[i],r=1/20,col=sample(1:70,1))
      circle(xx[i],yy[i],r=1/120,col="black")
   }
   
   x <- 1+1/3*cos(theta)
   y <- 1+1/3*sin(theta)
   
   xx <- x[seq(from=3,to=70,by=12)]
   yy <- y[seq(from=3,to=70,by=12)]
   
   for(i in 1:length(xx)){
      circle(xx[i],yy[i],r=1/20,col=sample(1:70,1))
      circle(xx[i],yy[i],r=1/120,col="black")
   }
   colors()
   
   elipsa(x0=1,y0=1,a=0.1,b=0.5,col="yellow2")
   elipsa(x0=1,y0=0.8,a=0.05,b=0.5,col="mediumblue")
   elipsa(x0=1,y0=0.6,a=0.1,b=0.4,col="mediumblue")
   elipsa(x0=1,y0=0.8,a=0.05,b=0.5,col="mediumblue",border=NA)
   
   elipsa(x0=1.02,y0=1.15,a=0.04,b=0.05,col="turquoise")
   
   polygon(c(1.03,1.1,1.04),
           c(1.21,1.1,1.1),col="red")
   
   circle(x0=0.99,y0=1.12,r=0.02,col="turquoise",border=NA)
   circle(x0=1.01,y0=1.15,r=0.01,col="black")
   
   lines(c(1.04,1.25),
         c(0.23,-1),lwd=2)
   lines(c(0.96,0.75),
         c(0.23,-1),lwd=2)
   circle(x0=1.09,y0=-0.08,r=0.05,lwd=2)
   circle(x0=0.91,y0=-0.08,r=0.05,lwd=2)
   
}

zwierz()
zwierz()
zwierz()
zwierz()

?heat.colors()




elipsa <- function(a=2,b=3,n=1000,x0=0,y0=0,...){
   
   teta <- seq(0, 2*pi, length=n+1)[-1]
   x <- x0 + a*cos(teta)
   y <- y0 + b*sin(teta)
   
   polygon(x, y, asp=1, ...)
   
}


circle <- function(x0, y0, r, n=100, ...) {
   
   theta <- seq(0, 2*pi, length=n+1)[-1]
   x <- x0+r*cos(theta)
   y <- y0+r*sin(theta)
   polygon(x, y, ...)
   
}

plot.new()
plot.window(c(0,2),c(0,2))

theta <- seq(-pi/3, pi+pi/3, length=70)
x <- 1+cos(theta)
y <- 1+sin(theta)

for(i in 1:length(theta)){
   lines(c(1,x[i]),
         c(1,y[i]))
}


xx <- x[seq(from=1,to=70,by=3)]
yy <- y[seq(from=1,to=70,by=3)]


for(i in 1:length(xx)){
   circle(xx[i],yy[i],r=1/30,col=sample(1:675,1))
   circle(xx[i],yy[i],r=1/120,col="black")
}


x <- 1+2/3*cos(theta)
y <- 1+2/3*sin(theta)

xx <- x[seq(from=2,to=70,by=3)]
yy <- y[seq(from=2,to=70,by=3)]

for(i in 1:length(xx)){
   circle(xx[i],yy[i],r=1/30,col=sample(1:675,1))
   circle(xx[i],yy[i],r=1/120,col="black")
}

x <- 1+1/3*cos(theta)
y <- 1+1/3*sin(theta)

xx <- x[seq(from=3,to=70,by=12)]
yy <- y[seq(from=3,to=70,by=12)]

for(i in 1:length(xx)){
   circle(xx[i],yy[i],r=1/30,col=sample(1:675,1))
   circle(xx[i],yy[i],r=1/120,col="black")
}


elipsa(x0=1,y0=1,a=0.1,b=0.5,col="brown")
elipsa(x0=1,y0=0.8,a=0.05,b=0.5,col="white")
elipsa(x0=1,y0=0.6,a=0.1,b=0.4,col="white")
elipsa(x0=1,y0=0.8,a=0.05,b=0.5,col="white",border=NA)

elipsa(x0=1.02,y0=1.15,a=0.04,b=0.05,col="white")

polygon(c(1.03,1.1,1.04),
        c(1.21,1.1,1.1),col="red")

circle(x0=0.99,y0=1.12,r=0.02,col="white",border=NA)
circle(x0=1.01,y0=1.15,r=0.01,col="black")

lines(c(1.04,1.25),
      c(0.23,-1),lwd=2)
lines(c(0.96,0.75),
      c(0.23,-1),lwd=2)
circle(x0=1.09,y0=-0.08,r=0.05,lwd=2)
circle(x0=0.91,y0=-0.08,r=0.05,lwd=2)



?colors()
topo.colors(1)




# palka <- function(x1,y1,x2,y2){
#    
#    lines(c(x1,x2),c(y1,y2))
#    
#    xx <- x1 + sqrt(x1^2+x2^2)/3
#    yy <- y1 + sqrt(y1^2+y2^2)/3
#    
#    xxx <- 2*(x1+x2)/3
#    yyy <- 2*(y1+y2)/3
#    
#    circle(xx,yy,r=sqrt((x1-x2)^2+(y1-y2)^2)/30,col="yellow")
#    circle(xx,yy,r=sqrt((x1-x2)^2+(y1-y2)^2)/130,col="black")
#    
#    circle(xxx,yyy,r=sqrt((x1-x2)^2+(y1-y2)^2)/24,col="green")
#    circle(xxx,yyy,r=sqrt((x1-x2)^2+(y1-y2)^2)/100,col="black")
#    
#    l <- lm(c(y1,y2)~c(x1,x2))$coefficients
#    
#    f <- function(x){
#       l[1]+l[2]*x
#    }
#    
#    a <- function(x){
#       l[1]+0.2+l[2]*x
#    }
#    
#    b <- function(x){
#       l[1]-0.2+l[2]*x
#    }
#    
#    s <- seq(from=x1,to=x2,length.out=12)
#    
#    for(i in 1:11){
#       lines(c(s[i],s[i],s[i+1]),
#             c(a(s)[i],f(s)[i],b(s)[i+1]))
#    }
#    
# }
# 
# palka(1,1,2,2)
# palka(0,2,1,1)
# palka(1,1,1,2)
# palka(1,1,1.5,2)
# palka(1,1,1.4,2)
# palka(1,1,0.4,2)
# palka(1,1,0.9,2)
# palka(1,1,1.4,2)





# x1 <- 0
# y1 <- 0
# 
# x2 <- 0.5
# y2 <- 2
# 
# lines(c(x1,x2),c(y1,y2))
# 
# xx <- (x1+x2)/3
# yy <- (y1+y2)/3
# 
# xxx <- 2*(x1+x2)/3
# yyy <- 2*(y1+y2)/3
# 
# circle(xx,yy,r=sqrt((x1-x2)^2+(y1-y2)^2)/30,col="yellow")
# circle(xx,yy,r=sqrt((x1-x2)^2+(y1-y2)^2)/130,col="black")
# 
# circle(xxx,yyy,r=sqrt((x1-x2)^2+(y1-y2)^2)/24,col="green")
# circle(xxx,yyy,r=sqrt((x1-x2)^2+(y1-y2)^2)/100,col="black")

# 
# 
# l <- lm(c(y1,y2)~c(x1,x2))$coefficients
# l
# 
# aaa1 <- tan(atan(l[2])+pi/4)
# bbb1 <- y1-aaa1*x1
# bbb11 <- y2-aaa1*x1
# 
# bbb111 <- seq(from=2,to=0,length.out=12)
# s <- seq(from=x1,to=x2,length.out=12)
# ss <- bbb111+aaa1*s
# 
# 
# aaa2 <- tan(atan(l[2])-pi/4)
# 
# f <- function(x){
#    l[1]+l[2]*x
# }
# 
# ss <- f(s)
# 
# a <- function(x){
#    l[1]+0.2+l[2]*x
# }
# 
# b <- function(x){
#    l[1]-0.2+l[2]*x
# }
# 
# a(s)
# points(s,a(s))
# points(s,ss)
# points(s,b(s))
# 
# for(i in 1:11){
#    lines(c(s[i],s[i],s[i+1]),
#          c(a(s)[i],f(s)[i],b(s)[i+1]))
# }











lines(c(s[2],s[2]),
      c(a(s)[2],ff(s)[2]))

points(s[2],a(s)[2],col="red")
points(s[2],ff(s)[2],col="green")

?lines

polygon(a(s),ff(s),b(s))


ff <- function(x){
   l[1]+aaa1*x
}

sg <- ff(s)

lines(s[25],sg[25])

l[2]

alfa <- atan(l[2])
tan(pi/4)

lines(c(s[4],s[4]),c(ss[4],ss[4]+0.2))

lines(c(s,s),c(ss,ss+0.2))

for(i in 1:length(s)){
   lines(c(s[i],s[i]),
         c(ss[i],ss[i]+0.2))
}

lines(c(s[5],aaa2*s[5]+l[2]),c(ss[5],aaa2*ss[5]+l[2]))

aaa1
aaa2


ss[3]

###############################################################################


parents <- c(NA,2,1,2,3,4,NA,7)
labels <- c("a","c","b","d","e","f","g","h")

psor <- sort(parents,na.last=FALSE)
r <- rle(psor)
lab <- labels[order(parents,na.last=FALSE)]

rl <- r$lengths
rv <- r$values

plot.new()

points(0,0)
points(0,1)
points(1,0)
points(1,1)
points(0.5,0.5)

for(i in 1:length(lab)){
   
   rl[i]
   
}

1/rl[1]

na <- length(which(is.na(parents)))

sr <- 1/(na+1)

circle(sr,0.9,0.1)
circle(2*sr,0.9,0.1)

###########################################################################

plot.new()
plot.window(c(-1,1),c(-1,1))

points(-1,1)
points(1,-1)
points(1,1)
points(-1,-1)
points(0,0)

parents <- c(NA,1,2,2,3,4,5,7)
labels <- c("academ","core v","commetmen","curriculum","honor","master","def","examples")

kolumny <- length(na.omit(unique(parents)))
wiersze <- 

rle(sort(parents))$lengths

#########################################################

to <- matrix(c("kupić jajka, mąkę, cukier","ubić pianę","wymieszać mąkę z wodą",
        "wszystko zamieszać",60,5,2,2,"",1,1,"2, 3"),nrow=4)
todo <- data.frame(to)

names(todo) <- c("task","length","dependson")

todo

wiersze <- nrow(todo)+1

start <- numeric(nrow(todo))
koniec <- numeric(nrow(todo))

for(i in 1:length(todo$dependson)){
   
   a <- todo$dependson[i]
   b <- as.numeric(as.character(todo$length[i]))
   
   if(a==""){
      start[i] <- 1
      koniec[i] <- b
   } else{
      odk <- stri_match_all_regex(a,"([\\p{N}]+)[,|$]?")[[1]][,2]
      odcz <- as.numeric(odk)
      start[i] <- max(koniec[odcz])+1
      koniec[i] <- start[i]+b-1
   }
}

start
koniec

kolumny <- max(koniec)

wiersze
kolumny

plot.new()
plot.window(c(-kolumny/6,kolumny),
            c(0,wiersze))
# points(0,0)
# points(0,wiersze)
# points(kolumny,0)
# points(kolumny,wiersze)
# points(-kolumny/6,0)
# points(-kolumny/6,wiersze)

#rect(-kolumny/6,wiersze-1,kolumny,wiersze,col="#00ff00d5")
rect(-kolumny/6,0,0,wiersze-1,col="darkorange")

s <- seq(0,kolumny,length.out=6)
ss <- seq(0,wiersze,by=1)

lines(c(-kolumny/6,-kolumny/6),c(0,wiersze-1))
lines(c(0,0),
      c(0,wiersze-0.9))
# for(i in 1:(kolumny+2)){
#    lines(c(s[i],s[i]),c(0,wiersze))
# }

for(i in 1:(wiersze)){
   lines(c(-kolumny/6,kolumny),c(ss[i],ss[i]))
}

for(i in 1:length(start)){
   lines(c(koniec[i],koniec[i]),
         c(0,wiersze-0.9))
   rect(start[i]-1,wiersze-i-1,koniec[i],wiersze-i,col="yellow")
}

start-1
kon <- c(0,koniec)
kon

for(i in 1:length(kon)){
   text(kon[i],wiersze-0.9,kon[i],pos=3)
}

s <- 1



stri_length(todo$task)

z <- character(length(todo$task))

for(i in 1:length(todo$task)){
   a <- stri_length(todo$task[i])
   if(a > 15){
      b <- unlist(stri_extract_all_regex(as.character(todo$task[i]),"^.{1,12}"))
      z[i] <- stri_paste(c(b,"..."),collapse="") 
   } else z[i] <- as.character(todo$task[i])
}

for(i in 1:(wiersze-1)){
   text(-kolumny/12,wiersze-1-1/2*s,z[i])
   s <- s+2
}

a <- stri_length(todo$task[i])
colors()

gantt <- function(todo,col1="darkorange",col2="yellow2"){
   
   wiersze <- nrow(todo)+1
   
   start <- numeric(nrow(todo))
   koniec <- numeric(nrow(todo))
   
   for(i in 1:length(todo$dependson)){
      
      a <- todo$dependson[i]
      b <- as.numeric(as.character(todo$length[i]))
      
      if(a==""){
         start[i] <- 1
         koniec[i] <- b
      } else{
         odk <- stri_match_all_regex(a,"([\\p{N}]+)[,|$]?")[[1]][,2]
         odcz <- as.numeric(odk)
         start[i] <- max(koniec[odcz])+1
         koniec[i] <- start[i]+b-1
      }
   }

   kolumny <- max(koniec)

   plot.new()
   plot.window(c(-kolumny/6,kolumny),
               c(0,wiersze))

   rect(-kolumny/6,0,0,wiersze-1,col=col1)
   
   s <- seq(0,kolumny,length.out=6)
   ss <- seq(0,wiersze,by=1)
   
   lines(c(-kolumny/6,-kolumny/6),c(0,wiersze-1))
   lines(c(0,0),
         c(0,wiersze-0.9))
   
   for(i in 1:(wiersze)){
      lines(c(-kolumny/6,kolumny),c(ss[i],ss[i]))
   }
   
   for(i in 1:length(start)){
      lines(c(koniec[i],koniec[i]),
            c(0,wiersze-0.9))
      rect(start[i]-1,wiersze-i-1,koniec[i],wiersze-i,col=col2)
   }
   
   kon <- c(0,koniec)
   
   for(i in 1:length(kon)){
      text(kon[i],wiersze-0.9,kon[i],pos=3)
   }
   
   s <- 1
   z <- character(length(todo$task))
   
   for(i in 1:length(todo$task)){
      a <- stri_length(todo$task[i])
      if(a > 15){
         b <- unlist(stri_extract_all_regex(as.character(todo$task[i]),"^.{1,12}"))
         z[i] <- stri_paste(c(b,"..."),collapse="") 
      } else z[i] <- as.character(todo$task[i])
   }
   
   for(i in 1:(wiersze-1)){
      text(-kolumny/12,wiersze-1-1/2*s,z[i])
      s <- s+2
   }
   
}

to <- matrix(c("kupić jajka, mąkę, cukier","ubić pianę","wymieszać mąkę z wodą",
               "wszystko zamieszać",60,5,2,2,"",1,1,"2, 3"),nrow=4)
todo <- data.frame(to)
names(todo) <- c("task","length","dependson")
gantt(todo)

to <- matrix(c("market research","define specifications","overall archiecture",
               "project planning","detail design","software dovelopment",
               "test plan","testing and QA","user documentation",
               2,2,2,2,2,5,2,3,2,"",1,2,3,3,5,6,5,8),nrow=9)
todo <- data.frame(to)
names(todo) <- c("task","length","dependson")
gantt(todo)

#######################################################################


function Simple(year,month,day)
{
   var lp = 2551443; 
   var now = new Date(year,month-1,day,20,35,0);						
   var new_moon = new Date(1970, 0, 7, 20, 35, 0);
   var phase = ((now.getTime() - new_moon.getTime())/1000) % lp;
   return Math.floor(phase /(24*3600)) + 1;
}

y <- 2013
m <- 12
d <- 7
lp <- 2551443

now <- 

##########################################################################


x <- seq(from=-5,to=5,length.out=1000)

mi <- 0
sigma <- 1
n <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

mi <- -2
sigma <- 0.5
nn <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

mi <- 0
sigma <- 5
nnn <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

lambda <- 1
nnnn <- numeric(length(x))
for(i in 1:length(x)){
   if(x[i]<0) nnnn[i] <- 0 else nnnn[i] <- lambda*exp(-lambda*x[i])
}
nnnn

nnnnn <- numeric(length(x))
lambda <- 1.5
for(i in 1:length(x)){
   if(x[i]<0) nnnnn[i] <- 0 else nnnnn[i] <- lambda*exp(-lambda*x[i])
}

y <- matrix(c(n,nn,nnn,nnnn,nnnnn),ncol=5)

m <- max(y)

plot.new()
plot.window(c(x[1]-1,x[length(x)]+1),
            c(-1,m+0.5))

palette <- rainbow(ncol(y),alpha=0.5)

for(i in 1:ncol(y)){
   w <- which(cumsum(y[,i])!=0)[1]
   polygon(c(x,x[length(x)],x[w]),c(y[,i],0,0),col=palette[i])
}

pod <- unique(floor(x))

for(i in 1:length(pod)){
   lines(c(pod[i],pod[i]),
         c(0,m+0.5),lty=3,lwd=0.2)
   text(pod[i],0,pod[i],pos=1,cex=0.7)
}

lines(c(x[1],x[length(x)]),
      c(0,0))
lines(c(x[1],x[1]),c(0,m+0.5))

f <- function(i){
   max(y[,i])
}

g <- c(unlist(lapply(1:ncol(y),f)),m+0.5)

for(i in 1:length(g)){
   lines(c(x[1],x[length(x)]),
         c(g[i],g[i]),lty=3,lwd=0.2)
   text(x[1],g[i],round(g[i],2),pos=2,cex=0.7)
}

multiplot <- function(x,y){
   
   stopifnot(is.numeric(x))
   stopifnot(length(x)>0)
   stopifnot(is.matrix(y))
   stopifnot(is.numeric(y))
   stopifnot(nrow(y)==length(x))
   stopifnot(all(sort(x)==x))
   
   m <- max(y)
   
   plot.new()
   plot.window(c(x[1]-1,x[length(x)]+1),
               c(-1,m+0.5))
   
   palette <- rainbow(ncol(y),alpha=0.5)
   
   for(i in 1:ncol(y)){
      w <- which(cumsum(y[,i])!=0)[1]
      polygon(c(x,x[length(x)],x[w]),c(y[,i],0,0),col=palette[i])
   }
   
   pod <- unique(floor(x))
   
   for(i in 1:length(pod)){
      lines(c(pod[i],pod[i]),
            c(0,m+0.5),lty=3,lwd=0.2)
      text(pod[i],0,pod[i],pos=1,cex=0.7)
   }
   
   lines(c(x[1],x[length(x)]),
         c(0,0))
   lines(c(x[1],x[1]),c(0,m+0.5))
   
   f <- function(i){
      max(y[,i])
   }
   
   g <- c(unlist(lapply(1:ncol(y),f)),m+0.5)
   
   for(i in 1:length(g)){
      lines(c(x[1],x[length(x)]),
            c(g[i],g[i]),lty=3,lwd=0.2)
      text(x[1],g[i],round(g[i],2),pos=2,cex=0.7)
   }
   
}

x <- seq(from=-5,to=5,length.out=1000)

mi <- 0
sigma <- 1
n <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

mi <- -2
sigma <- 0.5
nn <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

mi <- 0
sigma <- 5
nnn <- exp(-((x-mi)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))

lambda <- 1
nnnn <- numeric(length(x))
for(i in 1:length(x)){
   if(x[i]<0) nnnn[i] <- 0 else nnnn[i] <- lambda*exp(-lambda*x[i])
}

nnnnn <- numeric(length(x))
lambda <- 1.5
for(i in 1:length(x)){
   if(x[i]<0) nnnnn[i] <- 0 else nnnnn[i] <- lambda*exp(-lambda*x[i])
}

y <- matrix(c(n,nn,nnn,nnnn,nnnnn),ncol=5)

multiplot(x,y)







