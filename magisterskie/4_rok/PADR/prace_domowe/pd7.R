## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 07
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## ------------------------ Zadanie 07.01 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja BibTeX2data.frame(), która jako argument wejsciowy przyjmuje jeden
# napis fname okreslajacy sciezke dostepu do pliku zawierajacego 
# bibliografie w formacie Bibtexa, zwraca ramke danych. Kazdy wiersz ramki
# okresla inny wpis bibliograficzny, zas kolumny odpowiednio: typ wpisu,
# identyfikator wpisu, autora, tytul itd. (w zaleznosci od typu beda tu
# rozne kategorie).
#
# ARGUMENTY WEJSCIOWE
# 
# fname - pojedynczy napis okreslajacy sciezke dostepu (gdy taki plik nie 
#         istnieje, funkcja zwraca blad)
#
# WARTOSC ZWRACANA
# 
# ramka danych
#

## ---- Funkcja ----

BibTeX2data.frame <- function(fname){
   
   stopifnot(is.character(fname))
   stopifnot(file.exists(fname))
   
   tekst <- readLines(fname,warn=FALSE)
   
   a <- stri_paste(tekst,collapse="\n")
   b <- stri_replace_all_regex(a,"\\t"," ")
   bibl <- unlist(stri_split_regex(b,"(?m)^@"))[-1]
   
   bib <- stri_match_all_regex(bibl,"^(.*?)[{](.*?)[,]")
   bib2 <- lapply(1:length(bib),function(i){
      bib[[i]][,c(2,3)]
   })
   
   z <- vector("list",length(bibl))
   for(i in 1:length(bibl)){
      z[[i]] <- stri_match_all_regex(bibl[i],
                                     "(?m)^(.*?)[=][\\s|\"|{]*(.*?)[\\s|\"|}]*(?:[,])?[\n]")
   }
   
   u <- unlist(lapply(1:length(z),function(i){lapply(z[[i]],dim)}))
   wym <- matrix(u,ncol=2,byrow=TRUE)
   
   mac <- lapply(1:length(z),function(i){
      w <- unlist(z[[i]])
      w <- stri_trim_both(w)
      dim(w) <- wym[i,]
      w[,c(2,3)]
   })
   
   tyt <- lapply(1:length(mac),function(i){
      mac[[i]][,1]
   })
   
   u <- tyt[[1]]
   for(i in 2:length(tyt)){
      u <- union(tyt[[i]],u)   
   }
   
   tytuly <- c("type","id",u)
   
   tu <- matrix(NA,ncol=length(tytuly),nrow=length(mac)) 
   tu[1:nrow(tu),1:2] <- matrix(unlist(bib2),nrow=length(mac),byrow=TRUE)
   
   for(j in 1:length(mac)){
      g <- mac[[j]]
      for(i in 1:nrow(g)){
         w <- which(g[i,1]==tytuly)
         tu[j,w] <- g[i,2]
      }
   }
   
   d <- data.frame(tu)
   colnames(d) <- tytuly
   d
   
}

## ---- Przyklady ----


## ------------------------ Zadanie 07.03 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja file_split_regex(), ktora jako argument fname przyjmuje pojedynczy
# napis okreslajacy sciezke dostepu do pliku oraz regex - pojedynczy napis
# reprezentujacy wyrazenie regularne, rozdziela zawartosc pliku na czesci
# za pomoca danego wyrazenia regularnego (dopasowanie okresla granice 
# podzialu). Funkcja przyjmuje rowniez dodatkowy argument newline, ktory, 
# gdy jest rowny TRUE, rozdziela rowniez plik na czesci ze wzgledu na znak 
# nowej linii, a gdy FALSE, to tego nie robi. Kazdy z wydobytych fragmentow
# zapistywany jest w oddzielnym pliku w katalogu roboczym - do nazwy bazowej
# pliku wejsciowego doklejamy kolejny numer fragmentu.
#
# ARGUMENTY WEJSCIOWE
# 
# fname - pojedynczy napis
# regex - pojedynczy napis
# newline - wartosc logiczna (TRUE albo FALSE) - domyslnie (FALSE)
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

file_split_regex <- function(fname,regex,newline=FALSE){
   
   stopifnot(is.character(fname))
   stopifnot(is.character(regex))
   stopifnot(is.logical(newline))
   stopifnot(file.exists(fname))
   
   tekst <- readLines(fname)
   t <- stri_paste(tekst,collapse="\n")
   
   zz <- unlist(stri_split_regex(t,regex))
   
   if(newline==TRUE){
      mm <- stri_paste(zz,collapse="\n")
      zz <- unlist(stri_split_regex(mm,"\n"))
      if(length(which(zz==""))>0) zz <- zz[-which(zz=="")] 
   }
   
   for(i in 1:length(zz)){
      
      gdzie <- stri_paste(c(tempdir(),"/",i,".txt"),collapse="")
      gg <- normalizePath(gdzie,mustWork=FALSE)
      f <- file(gg,"w")
      writeLines(zz[i],f) 
      close(f)
      
   }
   
}


## ---- Przyklady ----



## ------------------------ Zadanie 07.04 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja indeks(), jako pierwszy argument przyjmuje wektor napisow
# files (kazdy napis okresla sciezke dostepu do pliku tekstowego na 
# dysku - jesli plik nie istnieje, funkcja zwraca blad), a jako drugi 
# argument - out - nazwe pliku, w ktorym maja byc zamieszczone wyniki. 
# Funkcja ma wydobyc z wczytanych plikow wszystkie spojne ciagi znakow 
# niebedacych bialymi ani przestankowymi. W pliku wyjsciowym zamieszcza 
# uporzadkowany leksykograficznie spis wszystkich ciagow znakow (kazdy w
# oddzielnym wierszu). Po kazdym slowie (po spacji) powinien znalezc sie 
# wykaz, w ktorych plikach i, po dwukropku, w ktorych wierszach dane slowo
# wystepuje (oddzielone przecinkami).
#
# ARGUMENTY WEJSCIOWE
# 
# files - wektor napisow
# out - pojedynczy napis (jesli sciezka dostepu, ktora on reprezentuje, nie 
#                         istnieje, to funkcja sama ja tworzy)
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

indeks <- function(files,out){
   
   stopifnot(is.character(files))
   stopifnot(is.character(out))
   stopifnot(length(files)>0)
   stopifnot(length(out)==1)
   stopifnot(all(file.exists(files)))
   
   if(!file.exists(out)) file.create(out)
   
   f <- normalizePath(files)
   
   tekst <- lapply(1:length(f),function(i){
      readLines(f[i],warn=FALSE)
   })
   
   sl <- function(i){ 
      na.omit(stri_extract_all_regex(tekst[[i]],"[\\d\\w-]*[\\w\\d$]"))
   }
   
   s <- lapply(1:length(f),sl)
   
   ileile <- unlist(lapply(1:length(f),function(i){ lapply(s[[i]],length)}))
   ile <- unlist(lapply(s,length))
   
   a <- rep(1:length(f),ile)
   plik <- rep(a,ileile)
   
   b <- vector("list",length(f))
   b[[1]] <- rep(1:ile[1],ileile[1:ile[1]])
   
   if(length(f)>1){
      for(i in 2:length(f)){
         b[[i]] <- rep(1:ile[i],ileile[(sum(ile[1:(i-1)])+1):sum(ile[1:i])])
      }
   }
   
   linia <- unlist(b)
   slowa <- unlist(s)
   
   ord <- order(slowa)
   sor <- sort(slowa,na.last=TRUE)
   
   splik <- plik[ord]
   slinia <- linia[ord]
   
   to <- unlist(stri_match_all_regex(files,"[/](\\w+[.]\\w+)"))
   dim(to) <- c(2,length(f))
   pl <- to[2,]
   
   ww <- which(is.na(sor))
   if(length(ww)>0){
      sor <- sor[-ww]
      slinia <- slinia[-ww]
      splik <- splik[-ww]
   }
   
   co <- character(length(sor))
   co[1] <- stri_paste(c(sor[1]," ",pl[splik[1]],":",slinia[1]),collapse="")
   
   for(i in 2:length(sor)){
      
      oo <- sor[1:(i-1)]
      
      if(any(sor[i] == oo)){
         w <- which(sor[i] == oo)
         co[w[1]] <-  stri_paste(c(co[w[1]],", ",
                                   pl[splik[i]],":",slinia[i]),collapse="")
      } else {
         co[i] <-  stri_paste(c(sor[i]," ",
                                pl[splik[i]],":",slinia[i]),collapse="")
      }
   }
   
   m <- which(co=="")
   if(length(m)>0) co <- co[-m]
   
   f <- file(out,"w")
   writeLines(co,f)
   close(f)
   
}

## ---- Przyklady ----


## ------------------------ Zadanie 07.05 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja dzielizapisuj(), ktora jako argument fname przyjmuje sciezke do 
# pliku tekstowego na dysku (jesli plik nie istnieje, funkcja zwraca blad),
# wydobywa z wczytanego pliku wszystkie spojne ciagi znakow nie bedace 
# bialymi ani przestankowymi. Poprawne slowa (ciagi znakow nie zawierajace
# cyfr) sa zapisywane w porzedku leksykograficznym do plikow na dysku o 
# nazwie okreslonej przez pierwsza litere slowa. Pozostale ciagi znakow (te,
# ktore zawieraja cyfry) sa zapisywane do pliku o nazwie "0.txt".
#
# ARGUMENTY WEJSCIOWE
# 
# fname - pojedynczy napis
#
# WARTOSC ZWRACANA
# 
# NULL
#

## ---- Funkcja ----

dzielizapisuj <- function(fname){
   
   stopifnot(is.character(fname))
   stopifnot(file.exists(fname))
   
   f <- normalizePath(fname)
   tekst <- readLines(f,warn=FALSE)
   sl <- unlist(stri_extract_all_regex(tekst,"[\\d\\w-]*[\\w\\d$]"))
   sl <- na.omit(sl)
   
   o <- stri_extract_first_charclass(sl,"Nd")
   w <- which(!is.na(o))
   inne <- sl[w]
   if(length(w)>0) sl <- sl[-w]
   
   lit <- unique(unlist(stri_extract_all_regex(
      stri_trans_tolower(sl),"^.")))
   
   f <- file("~/0.txt","w")
   writeLines(inne,f)
   close(f)
   
   for(i in 1:length(lit)){
      gdzie <- stri_paste(c("~/",lit[i],".txt"),collapse="")
      f <- file(gdzie,"w")
      to <- na.omit(unlist(stri_extract_all_regex(sl,
                        stri_paste(c("(?i)^",lit[i],".*"),collapse=""))))
      writeLines(to,f) 
      close(f)
   }
   
}

## ---- Przyklady ----



