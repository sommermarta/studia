#zad2

install.packages("AER")
library(AER)
library(car)

data(USMacroG)
USMacroG

MacroDiff=apply(USMacroG,2,diff)
MacroDiff
Macrodiff=data.frame(MacroDiff)
Macrodiff

pairs(MacroDiff[,c(-1,-3,-7,-8,-10,-11,-12)])    #wykres rozproszenia
# liniowy wp³yw na consumption ma dpi

model=lm(consumption~dpi+cpi+government+unemp,data=Macrodiff)
modelA=step(model,k=2,direction="backward")
summary(modelA)                                   #wyrzuci³ najpierw government, potem cpi, a zostawi³ unemp i cpi

AIC(model)-AIC(modelA)                          #ró¿nice miêdzy wielkoœciami funkcji kryterialnej
cor(MacroDiff[,c(-1,-3,-7,-8,-10,-11,-12)])      # korelacje
vif(model)                                       #wspó³czynniki podbicia wariancji 