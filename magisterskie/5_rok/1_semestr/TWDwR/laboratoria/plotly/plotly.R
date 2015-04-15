setwd("C:\\Users\\sommerm\\Dropbox\\TWDwR\\laboratoria\\plotly")

install.packages("devtools")  # so we can install from GitHub
devtools::install_github("ropensci/plotly")  # plotly is part of rOpenSci

library("plotly")
library("dplyr")

set_credentials_file(username = "przemyslaw.biecek", api_key = "zii1uj5shh") 

# tu moge sama sobie zalozyc za darmo konto, jesli chce
# haslo: TWD2015

# http://www.mortality.org/Public/ExplanatoryNotes.php#CompleteDataSeries

kobiety <- read.table("fltper_1x1.txt", skip=2, header=T) 
mezczyzni <- read.table("mltper_1x1.txt", skip=2, header=T) 

kobiety2 <- kobiety %>% 
  filter(Year %in% c(1958, 2005)) %>%
  mutate(Age = as.numeric(as.character(Age)),
         Gender = "F",
         Year=factor(Year))

mezczyzni2 <- mezczyzni %>% 
  filter(Year %in% c(1958, 2005)) %>%
  mutate(Age = as.numeric(as.character(Age)),
         Gender = "M",
         Year=factor(Year))

kobiety2mezczyzni <- kobiety2
kobiety2mezczyzni$qx <- kobiety2mezczyzni$qx / mezczyzni2$qx
kobiety2mezczyzni$Gender <- "F/M"

km2 <- rbind(kobiety2, mezczyzni2)

g1 <- ggplot(km2, aes(Age, qx, color=Year, group=Year)) + 
  geom_line(size=2) + geom_point() + 
  scale_y_continuous(trans="log10", breaks=c(0.0001,0.001,0.01,0.1,1), limits=c(0.0001,1)) +
  ylab("Prawdopodobieństwo zgonu w wieku x") + xlab("Wiek") + facet_wrap(~Gender)

g2 <- ggplot(kobiety2mezczyzni, aes(Age, qx, color=Year, group=Year)) + 
  geom_line(size=2) + geom_point() + 
  ylab("Prawdopodobieństwo zgonu w wieku x") + xlab("Wiek")

g1
g2

g3 <- ggplot(kobiety2, aes(x=Age, y=dx, color=Year, fill=Year)) + 
  geom_histogram(stat="identity", position = "dodge") +
  ylab("Zgonów na rok") + xlab("Wiek")

g3

kobiety30 <- kobiety %>% 
  filter(Age == 30)

g4 <- ggplot(kobiety30, aes(x=Year, y=30 + ex)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)+
  ylab("Oczekiwana długość życia 30 latki ") + xlab("Rok")

g4



c("h", "h", "s", "5", "j", "u", "1", "i", "i", "z") %>%
  rev() %>%
  paste(collapse="")


############################### nasz projekt!!! #################################################

kobiety2 <- kobiety %>% 
  filter(Age %in% as.character(0:5)) %>%
  mutate(Age = as.numeric(as.character(Age)),
         Gender = "F",
         zgony=dx) %>%
  group_by(Year) %>% 
  summarize(sumazgonow=sum(dx),
            plec='Kobiety')

head(kobiety2,15)

mezczyzni2 <- mezczyzni %>% 
  filter(Age %in% as.character(0:5)) %>%
  mutate(Age = as.numeric(as.character(Age)),
         Gender = "M",
         zgony=dx) %>%
  group_by(Year) %>% 
  summarize(sumazgonow=sum(dx),
            plec='Mężczyźni')

head(mezczyzni2,15)

d <- rbind(kobiety2,mezczyzni2)


wyk <- ggplot(d,aes(x=Year,y=sumazgonow,col=plec))+
  geom_point()+
  geom_line()+
  theme_bw()

wyk


















