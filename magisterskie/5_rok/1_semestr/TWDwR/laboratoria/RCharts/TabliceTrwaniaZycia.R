library("dplyr")
library("ggplot2")

setwd("C:\\Users\\sommerm\\Dropbox\\TWDwR\\RCharts\\G3")

# http://www.mortality.org/Public/ExplanatoryNotes.php#CompleteDataSeries

kobiety <- read.table("fltper_1x1.txt", skip=2, header=T)
mezczyzni <- read.table("mltper_1x1.txt", skip=2, header=T)

kobiety2005 <- kobiety %>%
  filter(Year == 2005) %>%
  mutate(Age = as.numeric(as.character(Age)))

ggplot(kobiety2005, aes(Age, qx)) +
  geom_line() +
  scale_y_continuous(trans="log10") +
  ylab("Prawdopodobieństwo zgonu w wieku x")

mezczyzni2005 <- mezczyzni %>%
  filter(Year == 2005) %>%
  mutate(Age = as.numeric(as.character(Age)))

# razem: (wiek jako zmienna dyskretna)

kobiety <- read.table("fltper_5x1.txt", skip=2, header=T)
mezczyzni <- read.table("mltper_5x1.txt", skip=2, header=T)

kobiety <- cbind(kobiety,plec=rep('K',nrow(kobiety)))
mezczyzni <- cbind(mezczyzni,plec=rep('M',nrow(mezczyzni)))

razem <- rbind(mezczyzni,kobiety)

razem2005 <- razem %>%
  filter(Year == 2005) %>%
  group_by(plec)

razem2005log <- razem %>%
  filter(Year == 2005) %>%
  mutate(qx = log(qx,10)+3.4) %>%
  group_by(plec)

ggplot(razem2005, aes(Age, qx, color=plec)) +
  geom_line() +
  scale_y_continuous(trans="log10") +
  ylab("Prawdopodobieństwo zgonu w wieku x")

razem2005 <- as.data.frame(razem2005)
razem2005log <- as.data.frame(razem2005log)
nPlot(qx ~ Age, group = "plec", data = razem2005, type = "multiBarChart")
nPlot(qx ~ dx, group = "plec", data = razem2005log, type = "cumulativeLineChart")

############################################## ostatecznie: #####################
# razem: (wiek jako zmienna ciagla)

kobiety <- read.table("fltper_1x1.txt", skip=2, header=T)
mezczyzni <- read.table("mltper_1x1.txt", skip=2, header=T)

kobiety <- cbind(kobiety,plec=rep('K',nrow(kobiety)))
mezczyzni <- cbind(mezczyzni,plec=rep('M',nrow(mezczyzni)))

razem <- rbind(mezczyzni,kobiety)

# razem2005 <- razem %>%
#   filter(Year == 2005) %>%
#   mutate(Age = as.numeric(as.character(Age))) %>%
#   group_by(plec)

razem2005log <- razem %>%
  filter(Year == 2005) %>%
  mutate(Age = as.numeric(as.character(Age))) %>%
  mutate(qx = log(qx,10)) %>%
  mutate(qx = round(qx-min(qx),2)) %>%
  group_by(plec)

# ggplot(razem2005, aes(Age, qx, color=plec)) +
#   geom_line() +
#   scale_y_continuous(trans="log10") +
#   ylab("Prawdopodobieństwo zgonu w wieku x")


# nPlot(qx ~ Age, group = "plec", data = razem2005, type = "multiBarChart")

# nPlot(qx ~ Age, group = "plec", data = razem2005log[-c(111,222),], 
#       type = "cumulativeLineChart")

nPlot(qx ~ Age, group = "plec", data = razem2005log[-c(111,222),], 
      type = "lineWithFocusChart")



