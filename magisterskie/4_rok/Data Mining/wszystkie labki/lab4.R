# 4.1

library("rpart")
library("MASS")
data(Cars93)
head(Cars93)
attach(Cars93)
Type

t <- as.character(Type)

for(i in 1:length(t)){
  if(t[i]=="Van" | t[i]=="Large") t[i] <- "D" else{
    if(t[i]=="Compact" | t[i]=="Midsize") t[i] <- "SR" else{
      if(t[i]=="Small") t[i] <- "M" else t[i] <- "SP"
    }
  }
}

t
c <- Cars93
head(c)
c$Typ <- t
attach(c)
head(c)

table(Typ)

c.tree <- rpart(Typ~Length+Weight+EngineSize+Horsepower+RPM, data=c, cp=0.0001,minsplit=5)

# minsplit - ile ma byc w wezle, zeby byly jeszcze tworzene podzialy
# cp - odpowiada za ucinanie drzewa

plot(c.tree)
text(c.tree,use.n=TRUE)

# klasy uporzadkowane leksykograficznie

print(summary(c.tree), digits=4)

c.tree_entr <- rpart(Typ~Length+Weight+EngineSize+Horsepower+RPM, 
                     data=c, cp=0.0001,minsplit=5,parms=list(split="information"))
plot(c.tree_entr)
text(c.tree_entr,use.n=TRUE)   # troszke sie rozni

# w praktyce nie ma roznicy, ktorej miary roznorodnosci sie uzyje - wiekszy wplyw ma wybor tego "cp"

# c)

# SR wyszedl patrzac na drzewko

m <- as.data.frame(t(apply(c[,c(12,13,14,19,25)],2,mean)))
predict(c.tree,newdata=m,type="class")

# d)

plotcp(c.tree)

# albo wybrac najmniejsza wartosc, albo regula 1SE -> wedlug tej reguly to 0.013
# te wasy to jedno odchylenie standardowe

# ucinamy sobie drzewo do wybranej przez nas wartosci cp:
z <- prune.rpart(c.tree,cp=0.07)
plot(z)
text(z,use.n=TRUE)


# 4.5 - drzewa regresyjne

f <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/fitness.txt",header=TRUE)
head(f)
attach(f)

# a)

f.tree <- rpart(Oxygen~.,data=f,cp=0.01,minsplit=2)
plot(f.tree)
text(f.tree,n.use=TRUE)

# b)

# dla takiego, co ma runtime < 8.9 i runpuls > 61

# c)

m <- as.data.frame(t(apply(f,2,median)))
m
predict(f.tree,newdata=m)  # 46.73

# d)

plotcp(f.tree) 

# e)

l <- lm(Oxygen~., data=f)
summary(l)
predict(l,m)

# rezydua:

sum(l$residuals^2)   # 128.8379 - dla modelu liniowego
sum(residuals(f.tree)^2)    # 23.82869 - dla drzewa

# mniejsze sse znaczy, ze model jest lepiej dopasowany do danych 
# wcale nie musi oznaczac to, ze ma lepsza predykcje

# f)

f.tree2 <- rpart(Oxygen~Age+RunTime,data=f,cp=0.02,minsplit=2)
plot(f.tree2)
text(f.tree2)









