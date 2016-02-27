# 10.0

set.seed(10)
x <- rnorm(20,5,2)
y <- rnorm(20,5,2)

dist(rbind(x,y),method="euclidean")
dist(rbind(x,y),method="maximum")
dist(rbind(x,y),method="canberra")
dist(rbind(x,y),method="minkowski",p=3)
                                                                                                                                                                                                                                                                                                                                     

# 10.1

dane <- dget("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/kwadraty.txt")
head(dane)
plot(dane)

cl1 <- kmeans(dist(dane),4,20)  # liczba skupien i liczba iteracji
skup1 <- as.numeric(cl1$cluster)
plot(dane,pch=skup1,col=skup1,main="kmeans")

# poeksperymentujmy:

cl <- kmeans(dist(dane),6,20)
skup <- as.numeric(cl$cluster)
plot(dane,pch=skup,col=skup,main="kmeans")

cl <- kmeans(dist(dane),2,20)
skup <- as.numeric(cl$cluster)
plot(dane,pch=skup,col=skup,main="kmeans")

cl1$withinss  # suma kwadratow odleglosci miedzy punktami w skupieniach

cl1$size  # liczba elementow w poszczegolnych skkupieniach

cl1

# wykres do wyboru liczby skupien:
s <- numeric(10)
for(i in 1:10){
  cl <- kmeans(dist(dane),i,20)
  s[i] <- sum(cl$withinss)
}

plot(1:10,s,type="b")   # to by sie zgadzalo :D na podstawie wykresu wybralibysmy 4

# metoda hierarchiczna:

hc <- hclust(dist(dane),method="average")
plot(hc)

# 1 - tyle skupien ile obserwacji
# 2 - laczymy najbardziej podobne skupienia
# przechodzimy coraz wyzej

# chcielibysmy uciac je na pewnym poziomie (ile chcemy skupien)

skup <- cutree(hc,k=4)
plot(dane,pch=skup,col=skup)

# dla single: (ta metoda dziala kiepsko)

hc <- hclust(dist(dane),method="single")
plot(hc)
skup <- cutree(hc,k=4)
plot(dane,pch=skup,col=skup)    # slabiutko :(


hc <- hclust(dist(dane),method="complete")
plot(hc)
skup <- cutree(hc,k=4)
plot(dane,pch=skup,col=skup)   # jest niezle!


# 10.3

install.packages("mclust")
library("mclust")

# nie zadajemy liczby skupien - ma wbudowane kryterium wyboru i sama sobie z tym radzi
mc <- Mclust(dane)
skup <- mc$classification
plot(dane,pch=skup,col=skup)  # dziala calkiem dobrze

mc$parameters

# 10.4

c <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/congress.txt",header=TRUE)
head(c)

heatmap(as.matrix(c),symm=TRUE)

hc <- hclust(as.dist(c),method="complete")
plot(hc)

# b)

loc <- cmdscale(as.dist(c))
x <- loc[,1]
y <- loc[,2]
plot(x,y,type="n")
text(x,y,names(c))

# reanaldo glosuje podobnie, jak demokraci
# sandman i thompson troche tacy jakby indywidualni (obserwacje odstajace)


# moglibysmy tu jeszcze zrobic analize skupien


# 10.2

w <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/wrecord.dat")
head(w)

w[,4:7] <- 60*w[,4:7]  # zeby wszystko bylo w sekundach
head(w)

st <- apply(w,2,sd)

for(i in 1:ncol(w)){
  w[,i] <- w[,i]/st[i]
}
head(w)

# a)

# b)

m <- as.matrix(w)
hc <- hclust(dist(m),method="complete")
plot(hc)

skupienia = cutree(hc,k=3) 

#Elementy pierwszego skupienia:
m[skupienia==1,]

#Srodki pierwszego skupienia:
m1 = apply(m[skupienia==1,],2,mean)
m2 = apply(m[skupienia==2,],2,mean)
m3 = apply(m[skupienia==3,],2,mean)

#b)

poczatkowe = rbind(m1,m2,m3)
km = kmeans(m,centers=poczatkowe)


#c)
Pca=princomp(m)


plot(Pca$scores[,1],Pca$scores[,2],col=km$cluster)












