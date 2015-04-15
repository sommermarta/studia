chooseBioCmirror() # bioconductor
setRepositories()
install.packages("gRbase")
install.packages("gRain", dependencies = TRUE)

#source("http://bioconductor.org/biocLite.R")
#biocLite(package_name)

# conditional probability table
library(gRain)
cpt1 <- cptable(~ parent1, levels = c("yes", "no"), values = c(0.2, 0.8))
cpt2 <- cptable(~ parent2, levels = c("yes", "no"), values = c(0.3, 0.7))
cpt3 <- cptable(~ y | parent1 + parent2, levels = c("yes", "no"),
                values = c(0.1, 0.9,
                           0.2, 0.8,
                           0.3, 0.7,
                           0.4, 0.6))
compiled_cpts <- compileCPT(list(cpt1, cpt2, cpt3))
g <- grain(compiled_cpts)
plot(g)

########################################################
## Earthquake
yn <- c("yes", "no")

e <- cptable(~ Earthquake, levels = yn, values = c(0.01, 0.99))
b <- cptable(~ BreakIn, levels = yn, values = c(0.02, 0.98))
a <- cptable(~ Alarm | Earthquake + BreakIn, levels = yn,
             values = c(0.95, 0.05,
                        0.94, 0.06,
                        0.29, 0.71,
                        0.001, 0.999))
en <- cptable(~ Neighbour | Alarm,
              levels = yn,
              values = c(0.7, 0.3,
                         0, 1))
r <- cptable(~ Radio | Earthquake,
             levels = c("No_Info", "Info"),
             values = c(0.7, 0.3,
                        0.99, 0.01))

cpts <- compileCPT(list(e, b, a, en, r))
print(cpts)
print(cpts$BreakIn)
print(cpts$Earthquake)
print(cpts$Alarm)
print(cpts$Neighbour)
print(cpts$Radio)

en <- grain(cpts)
plot(en)

querygrain(en, nodes = "Neighbour")
querygrain(en, nodes = c("Neighbour", "Alarm"), type="conditional")
querygrain(en, nodes = c("Neighbour", "Earthquake"), type="conditional")

enN <- setFinding(en, nodes = "Neighbour", states = "yes")
querygrain(enN, nodes = c("BreakIn", "Neighbour"))
querygrain(enN, nodes = "BreakIn")

enN2 <- setFinding(en, nodes = c("Neighbour", "Radio"), states = c("yes", "Info"))
querygrain(enN2, nodes = "BreakIn")

enN3 <- setFinding(en, nodes = "Alarm", states = "yes")
querygrain(enN3, nodes = c("BreakIn", "Earthquake"), type = "joint")
enN4 <- setFinding(en, nodes = "Alarm", states = "no")
querygrain(enN4, nodes = c("BreakIn", "Earthquake"), type = "joint")

# 2.4

n <- 10000

dane <- simulate(en, nsim = n)
head(dane)

# 1

sum(dane$Neighbour=="yes")/n
sum(dane$Neighbour=="no")/n

# 2

sum(dane$Neighbour=="yes" & dane$Alarm=="yes")/sum(dane$Alarm=="yes")
sum(dane$Neighbour=="yes" & dane$Alarm=="no")/sum(dane$Alarm=="no")
sum(dane$Neighbour=="no" & dane$Alarm=="yes")/sum(dane$Alarm=="yes")
sum(dane$Neighbour=="no" & dane$Alarm=="no")/sum(dane$Alarm=="no")

# ...

# exercise 2

cpt1 <- cptable(~ mother1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ mother2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt3 <- cptable(~ father1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt4 <- cptable(~ father2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt5 <- cptable(~ mother | mother1 + mother2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                        0, 0, 0, 1,
                                                                                        1, 0, 0, 0,
                                                                                        0, 0, 0, 1,
                                                                                        0, 1, 0, 0,
                                                                                        0, 1, 0, 0,
                                                                                        1, 0, 0, 0,
                                                                                        0, 1, 0, 0,
                                                                                        0, 0, 1, 0))
cpt6 <- cptable(~ father | father1 + father2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                        0, 0, 0, 1,
                                                                                        1, 0, 0, 0,
                                                                                        0, 0, 0, 1,
                                                                                        0, 1, 0, 0,
                                                                                        0, 1, 0, 0,
                                                                                        1, 0, 0, 0,
                                                                                        0, 1, 0, 0,
                                                                                        0, 0, 1, 0))
cpt7 <- cptable(~ child1 | mother1 + mother2, levels = c("A", "B", "0"), values = c(1, 0, 0,
                                                                                    0.5, 0.5, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0.5, 0,
                                                                                    0, 1, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0, 0.5,
                                                                                    0, 0.5, 0.5,
                                                                                    0, 0, 1))
cpt8 <- cptable(~ child2 | father1 + father2, levels = c("A", "B", "0"), values = c(1, 0, 0,
                                                                                    0.5, 0.5, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0.5, 0,
                                                                                    0, 1, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0, 0.5,
                                                                                    0, 0.5, 0.5,
                                                                                    0, 0, 1))
cpt9 <- cptable(~ child | child1 + child2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          0, 1, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          1, 0, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          0, 0, 1, 0))

# a a
# a b
# a 0
# b a
# b b
# b 0
# 0 a
# 0 b
# 0 0

compiled_cpts <- compileCPT(list(cpt1, cpt2, cpt3, cpt4, cpt5, cpt6, cpt7, cpt8, cpt9))
g <- grain(compiled_cpts)
plot(g)

pyt <- setFinding(g, nodes = c("child"), states = c("A"))
querygrain(pyt, nodes = "mother")

pyt <- setFinding(g, nodes = c("mother", "father"), states = c("0", "A"))
querygrain(pyt, nodes = "child")

# exercise 4

cpt1 <- cptable(~ prababciafm1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ prababciafm2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ prababciaff1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ prababciaff1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt1 <- cptable(~ prababciamm1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ prababciamm2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ prababciamf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ prababciamf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt1 <- cptable(~ pradziadekfm1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ pradziadekfm2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ pradziadekff1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ pradziadekff1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt1 <- cptable(~ pradziadekmm1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ pradziadekmm2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ pradziadekmf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ pradziadekmf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt1 <- cptable(~ dziadekm1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ dziadekm2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ dziadekf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ dziadekf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt1 <- cptable(~ babciam1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ babciam2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt1 <- cptable(~ babciaf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt2 <- cptable(~ babciaf1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))

cpt3 <- cptable(~ father1, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt4 <- cptable(~ father2, levels = c("A", "B", "0"), values = c(0.28, 0.06, 0.66))
cpt5 <- cptable(~ mother | mother1 + mother2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          0, 1, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          1, 0, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          0, 0, 1, 0))
cpt6 <- cptable(~ father | father1 + father2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          1, 0, 0, 0,
                                                                                          0, 0, 0, 1,
                                                                                          0, 1, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          1, 0, 0, 0,
                                                                                          0, 1, 0, 0,
                                                                                          0, 0, 1, 0))
cpt7 <- cptable(~ child1 | mother1 + mother2, levels = c("A", "B", "0"), values = c(1, 0, 0,
                                                                                    0.5, 0.5, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0.5, 0,
                                                                                    0, 1, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0, 0.5,
                                                                                    0, 0.5, 0.5,
                                                                                    0, 0, 1))
cpt8 <- cptable(~ child2 | father1 + father2, levels = c("A", "B", "0"), values = c(1, 0, 0,
                                                                                    0.5, 0.5, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0.5, 0,
                                                                                    0, 1, 0,
                                                                                    0.5, 0, 0.5,
                                                                                    0.5, 0, 0.5,
                                                                                    0, 0.5, 0.5,
                                                                                    0, 0, 1))
cpt9 <- cptable(~ child | child1 + child2, levels = c("A", "B", "0", "AB"), values = c(1, 0, 0, 0,
                                                                                       0, 0, 0, 1,
                                                                                       1, 0, 0, 0,
                                                                                       0, 0, 0, 1,
                                                                                       0, 1, 0, 0,
                                                                                       0, 1, 0, 0,
                                                                                       1, 0, 0, 0,
                                                                                       0, 1, 0, 0,
                                                                                       0, 0, 1, 0))






















































