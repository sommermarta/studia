## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 06
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 06.01 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja plhyphen(), przyjmujaca jako argument wektor napisow x 
# reprezentujacy kolejne akapity tekstu (jeden napis = jeden akapit) 
# napisanego w jezyku polskim, zwraca przeksztalcony wektor x, w taki 
# sposob, ze kazde slowo zostaje podzielone na sylaby wedlug uproszczonej
# reguly (separator wstawiamy przed kazda spolgloska, ktora poprzedza jakas
# samogloska i taka, ze wystepuja po niej jeszcze jakies samogloski). 
# Znak rozdzielajacy sylaby (domyslnie "-") przekazujemy za pomoca 
# argumentu hyphen.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow 
# hyphen - wektor napisow dlugosci 1
#
# WARTOSC ZWRACANA
# 
# wektor napisow
#

## ---- Funkcja ----

plhyphen <- function(x,hyphen="-"){
   
   stopifnot(is.character(x))
   stopifnot(length(x)>0)
   stopifnot(is.character(hyphen))
   stopifnot(length(hyphen)>0)
   stopifnot(stri_length(hyphen)>0)
   
   n <- length(x)
   ost <- character(n)
   
   f <- function(i){
      if(dlug[[i]] < 2) return("")
      c(rep(hyphen,dlug[[i]]-1),"")
   }
   
   g <- function(i){
      stri_paste(a[[i]],minusy[[i]])
   }
   
   
   for(i in 1:n){
      
      slowa <- unlist(stri_extract_all_regex(x[i],"[\\w]+[\\W]+"))
      
      a <- stri_extract_all_regex(slowa, 
               stri_paste("(([bcćdfghjklłmnńprsśtwzźżBCĆDFGHJKLŁMNŃPRSŚT",
                          "WZŹŻ]+)?[AĄEĘIOÓUYaąeęioóuy]+([$\\p{P}\\p{Zs}",
                          "\\n\\t]+)?([BCĆDFGHJKLŁMNŃPRSŚTWZŹŻbcćdfghjklł",
                          "mnńprsśtwzźż]+[$|\\p{P}|\\p{Zs}|\\n|\\t]+)?|[bc",
                          "ćdfghjklłmnńprsśtwzźżBCĆDFGHJKLŁMNŃPRSŚTWZŹŻ]+)",
                          collator=""))
      
      dlug <- lapply(a,length)
      
      minusy <- lapply(1:length(a),f)
      
      s <- lapply(1:length(a),g)
      ss <- unlist(s)
      ost[i] <- stri_paste(ss,collapse="")
      
   }
   
   ost
   
}

## ---- Przyklady ----

x <- c("Separator wstawiamy przed każdą spółgłoską, którą poprzedza jakaś
       samogłoska i taką, że występują po niej jeszcze jakieś samogłoski.",
       "Mam tak samo, jak ty!!! Miasto moje, a w niiiiiim!??? Najpiękniejszy
       mój świat!!!! Najpiękniejsze dni...",NA)
plhyphen(x)  #ok

x <- "jAn dębICA to jeSTEM ja???"
plhyphen(x)  #ok


## ------------------------ Zadanie 06.02 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja kalendarz(), przyjmujaca jako argument date postaci "yyyy-mm-dd",
# zwraca macierz calkowitoliczbowa reprezentujaca kalendarz na dany miesiac.
# Wartosciami sa sni danego miesiaca (dni spoza danego miesiaca
# reprezentowane sa przez NA). Nazwy kolumn wynikowej macierzy reprezentuja
# kolejne dni tygodnia, a nazwy wierszy - numery tygodni w roku.
#
# ARGUMENTY WEJSCIOWE
#
# x - pojedynczy napis postaci "yyyy-mm-dd" 
#
# WARTOSC ZWRACANA
# 
# macierz calkowitoliczbowa o 7 kolumnach
#

## ---- Funkcja ----

kalendarz <- function(x){
   
   stopifnot(is.character(x))
   stopifnot(length(x)==1)
   stopifnot(stri_length(x)==10)
   
   y <- as.POSIXlt(x)
   rok <- 1900 + y$year
   dziroku <- y$yday+1
   dzien <- y$mday
   mies <- y$mon+1
   
   if((rok%%4 == 0 & rok%%100 != 0) | rok%%400 == 0)#spr, czy rok przestepny
      przes <- TRUE else przes <- FALSE
   
   if(any(mies==c(1,3,5,7,8,10,12))){ #ile dni bedzie mial moj miesiac
      d <- 1:31 
   } else{
      if(mies==2 & przes){
         d <- 1:29
      }  else{
         if(mies==2 & !przes){
            d <- 1:28
         } else{
            d <- 1:30
         }
      } 
   }
   
   ad <- as.Date(x)
   pier <- ad-(dzien-1)
   pierw <- as.POSIXlt(pier)
   
   pd <- pierw$wday
   if(pd==0) pd <- 7
   
   na <- rep(NA,pd-1)
   
   dl <- 42-length(d)-length(na)
   naa <- rep(NA,dl)
   
   ccc <- c(na,d,naa)
   
   kal <- matrix(ccc,byrow=TRUE,ncol=7)
   
   if(!przes) dnimies <- c(31,28,31,30,31,30,31,31,30,31,30,31) else{
      dnimies <- c(31,29,31,30,31,30,31,31,30,31,30,31)
   }
   
   b <- as.POSIXlt(ad-(dziroku-1))$wday
   
   if (mies==1){   #ktory to tydzien
      if(b>=5) i <- 0 else i <- 1
   } else{
      if(b>=5) t <- 0 else t <- 1
      if(pd>=b) i <- cumsum(dnimies)[mies-1]%/%7 + t else{
         i <- cumsum(dnimies)[mies-1]%/%7 + 1 + t
      } 
   }
   
   tyg <- i:(i+5)
   
   dimnames(kal) <- list(tyg,c("Pn","Wt","Sr","Cz","Pt","So","Nd"))
   
   ost <- sum(is.na(kal[6,]))==7
   ostt <- ost & sum(is.na(kal[5,]))==7
   
   if(ostt) kal[1:4,] else if(ost) kal[1:5,] else kal
   
}

## ---- Przyklady ----

kalendarz("1813-11-12")  #ok
kalendarz("2013-11-12")  #ok
kalendarz("1602-02-12")  #ok
kalendarz("2011-08-12")  #ok

kalendarz("2010-01-01")  #ok
kalendarz("2010-02-12")  #ok
kalendarz("2010-03-12")  #ok
kalendarz("2010-04-12")  #ok
kalendarz("2010-05-12")  #ok
kalendarz("2010-06-12")  #ok
kalendarz("2010-07-12")  #ok
kalendarz("2010-08-12")  #ok
kalendarz("2010-09-12")  #ok
kalendarz("2010-10-12")  #ok
kalendarz("2010-11-12")  #ok
kalendarz("2010-12-12")  #ok


## ------------------------ Zadanie 06.03 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja findemails(), ktora jako argument przyjmuje wektor napisow x
# reprezentujacy kolejne paragrafy tekstu, zwroci wektor napisow, ktorego
# kolejne elementy zawierac beda wszystkie znalezione poprawne adresy email
# w wektorze x.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow
#
# WARTOSC ZWRACANA
# 
# wektor napisow
#

## ---- Funkcja ----

findemails <- function(x){
   
   stopifnot(is.character(x))
   stopifnot(length(x)>0)
   
   xx <- stri_paste(x,collapse=" ")
   s <- stri_extract_all_regex(xx, 
           stri_paste(c("[[A-Z][a-z][!#$%&'*+/=?^`{|}~]]([[A-Z][a-z][0-9]",
                        "[!#$%&'*+-/=?^_`{|}~][\\.]]+)?@[[A-Z][A-z][0-9]",
                        "[-][_][\\.]]+\\.[[A-Z][a-z]]{2,4}"),collapse=""))
   ss <- unlist(s)
   w <- which(stri_length(ss)>255)
   if(length(w)==length(x)) return(NA)
   if(all(!w)) ss else ss[-w]
   
}

## ---- Przyklady ----

x <- "Abc.example.com"
findemails(x)  #ok

x <- "A@b@c@example.com "
findemails(x)  #ok

x <- c("a@b@c","Send coments to Grzegorz.Urlich@math.sk",
       "ooo@e13.com, wu@ok.edu")
findemails(x)  #ok

findemails("Jola Lojalna <jl@google.com>")  #ok

x <- c("Banaszak Paula  Świetlica pbanaszak.sp118@gmail.com Bagińska Joanna
       Zerówka jbaginska.sp118@gmail.com Barsznica-Kalinowska Renata Pedagog
       rbarsznica.sp118@gmail.com", "Biegun-Rutkowska Magdalena Zerówka
       mbiegunrutkowska.sp118@gmail.com Bodkowska Dorota Edukacja 
       wczesnoszkolna dbodkowska.sp118@gmail.com")

findemails(x)  #ok

x <- "Jeśli ktoś zdecydowanie nie życzy sobie otrzymywać informacji z listy 
e-mailowej i chce zapobiec wpisaniu jego adresu przez kogoś innego - 
powinien zgłosić ten adres email do prowadzącego callbook - 
callbook@pzk.org.pl. Zablokowany email jest w bazie callbooka 
oznaczany czerwonym symbolem "

findemails(x)  #ok

x <- "mporttuner@primedia.com
info@injen.com
info@jic-magic.com
ee@kenwood.nl
comms@kenwood-electronics.co.uk
enquiries@kenwood-electronics.co.uk
info@konig.it
maxituning.redakcja@mpp.pl
tzimas@mpp.pl
info@momodesign.com
info@neuspeed.com
info@nitrousexpress.com
josh@nitrousexpress.com
info@powerslot.com
sales@powerslot.com
sales@revhard.com
support@revhard.com
sales@revotechnik.com
info@rojawheels.com
sales@rvmags.com
sales@skunk2.com
info@sparco.it
sparco06france@wanadoo.fr
info@sparcousa.com
maria.jamison@primedia.com
mweiss@stoptech.com
nburkoff@stoptech.com"

findemails(x)  #ok

x <- stri_paste(c("alwrhalh8974wyowh4uknseriulgsyorynhgsioty847ybsy7ouysob",
                  "t487aglnhlakjhaheagiuna74yrew7yqrwuefuiahfakjlgshl3vq48",
                  "759b4vq84qomcy7q4obvq4oq8yqqpontcyq47ytq748qov7q4vq6q3b",
                  "6q8u4584sdgf42t34gsdfwhesqagsdfxgarszdfxvcaszdfvxcahevl",
                  "valenvhalhpwoehgpqw2893u8y452-_.@alkdfj1.pl")
                ,collapse="")

findemails(x)  #NA - bo za dlugi



## ------------------------ Zadanie 06.04 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja decomposeurls(), ktora jako argument przyjmuje wektor napisow x,
# zwraca ramke danych o length(x) wierszach, zawierajaca - jako wektory
# napisow - rozlozone na odpowiednie fragmenty poprawne adresy URL. 
# Kolumny nosza nazwe odpowiednio: protokol, host i zasob. Jesli dany napis
# nie odpowiada poprawnym URL, odpowiadajacy wiersz ramki danych powinien
# zawierac braki danych.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow
#
# WARTOSC ZWRACANA
# 
# ramka danych o length(x) wierszach i trzech kolumnach 
#

## ---- Funkcja ----

decomposeurls <- function(x){
   
   stopifnot(is.character(x))
   stopifnot(length(x)>0)
   
   a <- stri_match_all_regex(x,
            stri_paste(c("([\\p{Ll}]+(?:[\\p{Ll}\\p{Nd}\\.-]+)?)[:][/]{2}",
                         "([^/]+)(?:[/])?([^\\s]+)?"),collapse=""))
   
   n <- length(x)
   m <- matrix(0,ncol=4,nrow=n)
   
   for(i in 1:n){
      m[i,] <- a[[i]]
   }
   
   mm <- m[,-1]                                                                                                           
   colnames(mm) <- c("protokol","host","zasob")
   w <- which(is.na(mm[,3]))
   ww <- which(is.na(m[w,1]))
   
   if(length(w)>0 & length(ww)>0) mm[w[-ww],3] <- ""
   data.frame(mm)
   
}

## ---- Przyklady ----

x <- c("http://www.wp.pl/jakis_katalog/jakis_katalog2/jakas_strona.html",
       "http://gagolewski.rexamine.com/publications/","ala mam kota",
       "ftp://231.135.44.35","http://www.wikipedia.com/wiki/URL",
       stri_paste(c("foo://username:password@example.com:8042/over/there",
                    "/index.dtb?type=animal&name=narwhal#nose"),
                  collapse=""),
       "acap://[<user>[;AUTH=<type>]@]<host>[:<port>]/<entry>",
       "http://userguide.icu-project.org")


as <- decomposeurls(x)
as   #ok


## ------------------------ Zadanie 06.05 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja dataextract(), ktora jako argument przyjmuje ramke danych x oraz
# pojedynczy napis what, w ktorym znajduja sie - jako podnapisy - ciagi
# znakow postaci "nazwa='wartosc'", zwraca podzbior x, ktorego kolumny nazwa
# maja podane wartosci. Jesli w what wcale nie ma ograniczen, zwracany jest
# x w swej oryginalnej postaci.
#
# ARGUMENTY WEJSCIOWE
# 
# x - ramka danych
# what - pojedynczy wektor napisow
#
# WARTOSC ZWRACANA
# 
# ramka danych 
#

## ---- Funkcja ----

dataextract <- function(x,what){
   
   stopifnot(is.character(what))
   stopifnot(length(what)==1)
   stopifnot(is.data.frame(x))
   
   naz <- names(x)
   m <- stri_match_all_regex(what,"([^\\p{Zs}]+)[=][']([^\\p{Zs}]+)[']")
   mm <- m[[1]]
   
   if(is.na(mm[1,1])) return(x)
   
   int <- mm[,2]
   f <- function(i){
      naz==int[i]
   }
   
   k <- lapply(1:length(int),f)
   
   if(all(unlist(k)==FALSE)) return(x)
   
   kk <- lapply(k,which)
   
   g <- function(i){
      if(length(kk[[i]])>0) i
   }
   
   w <- unlist(lapply(1:length(kk),g))
   
   if(length(w)==0) dane <- mm else dane <- mm[w,]
   dane <- matrix(dane,ncol=3,nrow=length(w))
   
   for(i in 1:nrow(dane)){
      
      ww <- which(dane[i,2]==naz)
      jak <- dane[i,3]
      x <- x[x[,ww]==jak,]
      
   }
   
   x
   
}

## ---- Przyklady ----

library("MASS")
s <- Cars93[,c("Origin","DriveTrain","MPG.city","Type")]
what <- stri_paste(c("SELECT * FROM s WHERE Origin='USA' i Ja='ghgu8'",
                   "AND DriveTrain='4WD' i Ja='ghgu8'"),collapse="")
dataextract(s,what)  #ok

what <- "Daj mi Origin='USA', DriveTrain='4WD', MPG.city='15'"
dataextract(s,what)  #ok

what <- "Origin='USA' DriveTrain='4WD' MPG.city='15' Type='Sporty'"
dataextract(s,what)  #ok

what <- "Chciałabym wybrać samochody, które mają Type='Sporty'"
dataextract(s,what)  #ok

what <- "cgvhhhhhhcfgxdjrddy565467r6yfukj="
dataextract(s,what)  #ok

what <- "cgvhhhhhh Ja='ghgu8' cfgxdjrddy565467r6yfukj="
dataextract(s,what)  #ok


