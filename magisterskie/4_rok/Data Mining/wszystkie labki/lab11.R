# Reguly asocjacyjne - przykladowa sesja w R:

#Przyklad 1

library("arules")

data("AdultUCI")

dim(AdultUCI)
head(AdultUCI)

#Przygotowanie danych:
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15,
                                                      25, 45, 65, 100)), labels = c("Young", "Middle-aged",
                                                                                    "Senior", "Old"))
AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]],
                                            c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time",
                                                                               "Over-time", "Workaholic"))
AdultUCI[["capital-gain"]] <- ordered(cut(AdultUCI[["capital-gain"]],
                                          c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] >
                                                                                         0]), Inf)), labels = c("None", "Low", "High"))
AdultUCI[["capital-loss"]] <- ordered(cut(AdultUCI[["capital-loss"]],
                                          c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] >
                                                                                         0]), Inf)), labels = c("none", "low", "high"))
head(AdultUCI)

#Tworzymy macierz transakcji:
Adult <- as(AdultUCI, "transactions")
Adult


#Itemsety:
itemFrequencyPlot(Adult)

#Itemsety z supportem co najmniej 0.1
itemFrequencyPlot(Adult, support = 0.1, cex.names = 0.8)

#Szukamy regul asocjacyjnych o minimalnym wsparciu i wiarygodnosci:
rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))

summary(rules)

#Zwykle zbior regul jest bardzo duzy, mozemy ogranczyc wyniki do tych w ktorych nastepnikiem jest zmienna Income

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" &
                             lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" &
                             lift > 1.2)

#Sortujemy wyniki:
inspect(head(sort(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"), n = 3))


#Zadanie 2
#Reguly asocjacyjne- wykorzystanie dla danych Breast Cancer Winconsin:

Dane = read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/breast-cancer-wisconsin.data",h=T,sep=",")
Dane = Dane[,-1]

#Konieczna jest dyskretyzacja zmiennych:
for(j in 1:ncol(Dane)) Dane[,j] =as.factor(Dane[,j])

Dane1 = as(Dane,"transactions")

# Wizualizacja macierzy transakcji (nie wykonywac dla duzch macierzy):
image(Dane1)

#Itemsety:
itemFrequencyPlot(Dane1)

#Itemsety z supportem co najmniej 0.1
itemFrequencyPlot(Dane1, support = 0.1, cex.names = 0.8)

#Szukamy regul asocjacyjnych o minimalnym wsparciu i wiarygodnosci:
rules <- apriori(Dane1, parameter = list(support = 0.01, confidence = 0.6))

summary(rules)

#Zbior regul jest bardzo duzy, mozemy ogranczyc wyniki do tych w ktorych nastepnikiem jest zmienna Class

rulesClassmalignant <- subset(rules, subset = rhs %in% "Class=malignant" &
                                lift > 1.2)


#Sortujemy wyniki:
inspect(head(sort(rulesClassmalignant, by = "confidence"), n = 3))


