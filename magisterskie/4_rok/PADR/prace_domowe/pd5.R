## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Programowanie i analiza danych w R 2013/2014
## Praca domowa nr 05
##
## Prowadzacy:    Cena Anna 
## Student:       Sommer Marta 237503
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


## ------------------------ Zadanie 05.01 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja genpwd() generuje n k-znakowych losowych, trudnych do zlamania 
# hasel. Trudne do zlamania haslo to takie, ktore sklada sie z co najmniej
# jednej malej oraz jednej wielkiej litery alfabetu lacinskiego, a takze z
# co najmniej jesnej cyfry i chociaz jednego znaku spoza wymienionych 
# zbiorow.
#
# ARGUMENTY WEJSCIOWE
# 
# n - liczba naturalna (domyslnie 1)
# k - liczba naturalna wieksza lub rowna 4 (domyslnie 8)
# dodatkowe - niepusty wektor napisow (kazdy napis dlugosci 1) - domyslnie 
#             dodatkowe = c("_","-")
#
# WARTOSC ZWRACANA
# 
# wektor napisow dlugosci n 
#


## ---- Funkcja ----

genpwd <- function(n=1,k=8,dodatkowe=c("_","-")){
   
   stopifnot(is.numeric(n))  
   stopifnot(is.numeric(k))  
   stopifnot(is.character(dodatkowe))   
   stopifnot(length(n)==1)   
   stopifnot(length(k)==1)
   stopifnot(length(dodatkowe)>0)
   stopifnot(all(stri_length(dodatkowe)==1))
   stopifnot(floor(n)==n)
   stopifnot(floor(k)==k)
   stopifnot(k>=4)
   
   f <- function(i){
      numeric(k)
   }
   
   ost <- lapply(1:n,f) #tworze n wektorow dlugosci k - na nich bede 
                        #zapisywac szukane hasla 
   
   z <- unlist(stri_enc_toutf32(dodatkowe))  #dodatkowe znaki w utf
   
   d <- 65:90   #duze litery w utf
   m <- 97:122  #male litery w utf
   l <- 48:57   #liczby w utf
   
   stopifnot(all(is.element(c(d,m,l),z)==FALSE))  #zeby znaki z dodatkowe 
                                                  #nie byly ani mala litera
                                                  #ani duza litera, ani 
                                                  #cyfra
   
   ff <- function(i){
      c(sample(d,1),sample(m,1),sample(l,1),sample(z,1))
   }
   
   co <- lapply(1:n,ff)  #losuje po jedej literze, liczbie i znaku dla 
                         #kazdego hasla
   
   fff <- function(i){
      sample(1:k,4)
   }
   
   gdzie <- lapply(1:n,fff)  #losuje miejsca w hasle, na ktorych mam je 
                             #umiescic
   
   g <- function(i){
      ost[[i]][gdzie[[i]]] <- co[[i]]
      ost[[i]]
   }
   
   to <- lapply(1:n,g)   #i je tam umieszczam
   
   gg <- function(i){
      sample(c(d,m,l,z),k-4,replace=TRUE)
   }
   
   r <- lapply(1:n,gg)   #losuje pozostala potrzebna ilosc elementow - tym
                         #razem juz z calego zbioru liter, liczb i znakow
   
   ggg <- function(i){
      to[[i]][-gdzie[[i]]] <- r[[i]]
      to[[i]]
   }
   
   toto <- lapply(1:n,ggg)   #umieszczam je na pozostale miejsca
   
   h <- function(i){
      stri_enc_fromutf32(toto[[i]])
   }
   
   unlist(lapply(1:n,h))  #zamieniam wszystko spowrotem na napisy
    
}

## ---- Przyklady ----

genpwd()               #ok
genpwd(4,5)            #ok
genpwd(3,7,c("%","(","#","_"))   #ok
genpwd(3.2)       #error
genpwd(2,4.4)     #error
genpwd(2,1)       #error
genpwd(5,4,c(";","_"))  #ok
genpwd(5,4,c("d",";"))  #error
genpwd(5,4,c(";;",";")) #error


## ------------------------ Zadanie 05.02 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja uncomment() usuwa komentarze z plikow zrodlowych roznych jezykow
# programowania. Jako argumenty przyjmuje ona wektor napisow tekst (kazdy
# napis oznacza jeden wiersz) oraz dwukolumnowa macierz znaczniki (pierwsza
# kolumna oznacza, w jaki sposob denotuje sie poczatek komentarza, a druga
# okresla, jak sie go konczy; ewentualna wartosc NA w drugiej kolumnie
# oznacza, ze komentarze trwaja do konca wiersza). 
#
# ARGUMENTY WEJSCIOWE
# 
# tekst - wektor napisow
# znaczniki - dwukolumnowa macierz napisow 
#
# WARTOSC ZWRACANA
# 
# wektor napisow o dlugosci takiej, jak wektor wejsciowy tekst
#

## ---- Funkcja ----

uncomment <- function(tekst,znaczniki){
   
   stopifnot(is.character(tekst))
   stopifnot(length(tekst)>0)
   stopifnot(is.matrix(znaczniki))
   stopifnot(ncol(znaczniki)==2)
   stopifnot(is.character(znaczniki))
   
   n <- length(tekst)  
   b <- character(n)   #pusty wektor, w którym bede zapisywac wyniki koncowe
   
   for(k in 1:n){    #przechodze po wszystkich elementach tekstu
      
      tek <- tekst[k]
      
      repeat{    #powtarzam dopoki...
         
         po <- stri_locate_first_fixed(tek,znaczniki[,1]) #szukam pierwszego 
                                                          #miejsca w tek, na
                                                          #ktorym pojawi sie 
                                                          #jakis otwarty
                                                          #nawias
         
         napo <- na.omit(po)
         if(length(napo)==0) break #jesli nie ma zadnego, to przerywam petle
         
         w <- which(po==min(napo))#szukam minimum wsrod pierwszych otwartych
                                  #nawiasow roznych rodzajow
         w <- w[1]
         popo <- po[w,]    #miejsce, gdzie byl pierwszy jakikolwiek otwarty
                           #nawias
         
         z <- znaczniki[w,2]   #jak wyglada domkniecie tego nawiasu
         koko <- stri_locate_first_fixed(tek,z) #gdzie znajduje sie pierwsze
                                                #domkniecie tego nawiasu
         mi <- min(popo)   #poczatek nawiasu otwartego
         ma <- max(koko)   #koniec nawiasu zamknietego
         
         teu <- unlist(stri_enc_toutf32(tek)) #tek zapisany w utf
         
         if(!is.na(ma)) ty <- teu[-(mi:ma)] else{
            ty <- teu[-(mi:length(teu))]   #usuwam z tek miejsca od mi do ma
         } 
         
         tek <- stri_enc_fromutf32(ty)  #i zamieniam z powrotem na napis
      }
      
      b[k] <- tek
   }
   
   b
   
}

## ---- Przyklady ----

t <- c("1 ///* tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg",
       "tup tup /* tup // aa */ 2 /*222*/gh //hajdgkjsagafjg")
z <- matrix(c("/*","//","*/",NA),ncol=2)

uncomment(t,z)  #ok

t <- c("hgahjagh7367#jshgdjkadhk","#jjjjjj")
z <- matrix(c("#",NA),ncol=2)

uncomment(t,z)  #ok :)

t <- c("hhhfggg","#jjjjjj")
z <- matrix(c("#",NA),ncol=2)

uncomment(t,z)  #ok :)


## ------------------------ Zadanie 05.03 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja sprNawiasy() sprawdza, czy wszystkie nawiasy (roznego rodzaju) w
# danych napisach sa poprawnie pozamykane i zagniezdzone. Jako argumenty
# przyjmuje wektor napisow tekst i dwukolumnowa macierz napisow 
# jednoznakowych nawiasy (pierwsza kolumna okresla nawiasy otwierajace, a 
# druga odpowiadajace im wersje zamykajace). Dla kazdego napisu w tekscie,
# funkcja zwraca 0, gdy nawiasy sa podane w sposob poprawny, lub numer 
# znaku, na ktorym lezy pierwszy nawias sprawiajacy problem.
#
# ARGUMENTY WEJSCIOWE
# 
# tekst - wektor napisow
# nawiasy - dwukolumnowa macierz jednoznakowych napisow (domyslnie 
#           nawiasy=matrix(c("(","{","[",")","}","]"),ncol=2))
#
# WARTOSC ZWRACANA
# 
# calkowitoliczbowy wektor o dlugosci rownej dlugosci wektora tekst
#

## ---- Funkcja ----

sprNawiasy <- function(tekst,nawiasy=matrix(c("(","{","[",")","}","]"),ncol=2)){
   
   stopifnot(is.character(tekst))
   stopifnot(length(tekst)>0)
   stopifnot(all(stri_length(tekst)>0))
   stopifnot(is.character(nawiasy))
   stopifnot(is.matrix(nawiasy))
   stopifnot(ncol(nawiasy)==2)
   stopifnot(all(stri_length(nawiasy)==1))
   
   n <- length(tekst)
   
   u <- unlist(stri_enc_toutf32(nawiasy))
   dim(u) <- dim(nawiasy)   # moje u to nawiasy zapisane w utf
   
   y <- numeric(n)  #tu bede umieszczac wyniki
   
   for(k in 1:n){   #dla kazdego napisu z tekstu
      
      t <- unlist(stri_enc_toutf32(tekst[k]))  #zapisuje go w postaci utf
      lt <- length(t)
      
      o <- numeric(nrow(nawiasy))   #ile jest juz otwartych nawiasow kazdego
                                    #rodzaju
      z <- numeric(nrow(nawiasy))   #ile jest juz zamknietych nawiasow
                                    #kazdego rodzaju
      
      for(i in 1:lt){   #przechodze po koleji po kazdym znaku napisu
         if(!is.element(t[i],u)) next else{  #jesli znak nie jest moim 
                                             #nawiasem, to dalej
            if(is.element(t[i],u[,1])){   #jesli jest nawiasem otwierajacym
               w <- which(t[i]==u[,1])    #to sprawdzam ktorym
               o[w] <- o[w] + 1           #i odpowiednio zwiekszam liczbe 
                                          #nawiasow otwartych tego rodzaju
            } else{                       #analogicznie dla zamknietych
               w <- which(t[i]==u[,2]) 
               z[w] <- z[w] + 1
            }
         }
         
         if(any(z > o)){   #jesli liczba zamknietych nawiasow przekroczy
                           #liczbe otwartych to ten indeks bedzie pierwszym 
                           #nieprawidlowym miejscem
            y[k] <- i
            break
         } else if(all(o==z)){   #jesli wszystkich jest po rowno to 0, bo 
                                 #jest, jak na razie ok
            y[k] <- 0 
         } else y[k] <- i    #jesli nie jest po rowno, ani nie jest wiecej
                             #zamknietych tzn, ze blad jest na koncu
         
      }      
   }
   
   y
   
}

## ---- Przyklady ----

sprNawiasy("(())")    #ok
sprNawiasy("[(())]")  #ok
sprNawiasy("aaa(())") #ok
sprNawiasy("(())[sasa")  #ok
sprNawiasy("(())]ss")    #ok
sprNawiasy("(())[ss]")   #ok
sprNawiasy("a({22vaf{f}ffaga}")  #ok
sprNawiasy("[(a {((b)(c))}(d)e)]")  #ok
sprNawiasy(c("[[]{}ddd(()","{[(a ((b)(c))(d)e)]}"))  #ok
sprNawiasy("(")   #ok
sprNawiasy("a((b)a)",matrix(c("a","b"),ncol=2))  #ok

sprNawiasy("",matrix(c("a","b"),ncol=2))   #error
sprNawiasy("aabbabbbbbb",matrix(c("a","b"),ncol=2))   #ok
sprNawiasy("aabbabbbbbb",matrix(c("a","bb"),ncol=2))  #error   


## ------------------------ Zadanie 05.07 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja nbsp(), ktora jako argument przyjmuje wektor napisow x oraz 
# pojedynczy napis nbspchar, w kazdym napisie z x najpierw zastepuje 
# wszystkie spojne ciagi bialych znakow pojedynczymi spacjami, a nastepnie
# wykrywa wszystkie jedno- lub dwuliterowe slowa i zamiast wystepujacej po
# niej ewentualnie spacji wstawia nbspchar.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor napisow
# nbspchar - pojedynczy napis
#
# WARTOSC ZWRACANA
# 
# wektor napisow o dlugosci rownej dlugosci wektora x 
#

## ---- Funkcja ----

nbsp <- function(x,nbspchar){
   
   stopifnot(is.character(x))
   stopifnot(is.character(nbspchar))
   stopifnot(length(x)>0)
   stopifnot(length(nbspchar)==1)
   
   n <- length(x)  
   bezspac <- character(n)   #tu beda ostatecznie zdania bez spacji
   ostat <- character(n)     #a tu te i bez spacji i z nbspchar-em
   
   f <- function(i){    
      if(dlug[[i]]<=2) l[[i]] <- nbspchar
      l[[i]]
   }
   
   ff <- function(i){
      stri_paste(aaa[[i]],znaki[[i]])
   }
   
   for(k in 1:n){
      
      xx <- stri_trim_both(x[k])  #usuwam spacje z poczatku i konca najpierw
      ut <- unlist(stri_enc_toutf32(xx))  #zamieniam zdanie na utf
      lut <- length(ut)
      p <- logical(lut-1)   #wektor pomocniczy
      
      for(i in 1:(lut-1)){ #jesli jest wiecej niz jedna spacja, to
                           #mijsce, w ktorym sie tak zdazylo zapisuje w 
                           #wektorze p
         if(ut[i]==32 & ut[i+1]==32) p[i+1] <- TRUE
      }
      
      w <- which(p)    
      uw <- ut[-w]  #wyrzucam z mojego zdania te, ktore sie powtarzaly 
      
      bezspac[k] <- unlist(stri_enc_fromutf32(uw))  #i zamieniam na napis
      
      aaa <- as.list(unlist(stri_split_fixed(bezspac[k]," ")))#dziele zdanie
                                                              #(juz z 
                                                              #poprawnymi
                                                              #spacjami) na
                                                              #poszczegolne 
                                                              #wyrazy
      dlug <- lapply(aaa,stri_length)    #sprawdzam ich dlugosc
      
      l <- as.list(rep(" ",length(dlug))) #tworze pusta liste napisow - bede
                                          #w niej zapamietywac, gdzie wyrazy
                                          #byly krotkie i powinny zamiast
                                          #spacji miec potem nbspchar
      
      znaki <- lapply(1:length(dlug),f)  
      
      y <- lapply(1:(length(dlug)-1),ff)  #lacze krotkie wyrazy i nbspchar-y
      y[[length(dlug)]] <- aaa[[length(dlug)]]
      uy <- unlist(y)
      
      ostat[k] <- stri_paste(uy,collapse="") #no i lacze wszystko w jeden
                                             #napis
      
   }
   
   ostat
   
}

## ---- Przyklady ----

x <- c("    Na zielonym          œwietle     hop-hop, a na czerwonym prr. ",
       "jestê     zielon¹ traw    k¹ j¹!!!!  ")
nb <- "~"

nbsp(x,nb)  #ok

x <- c("    Na zielonym    œwietle     hop-hop, a na czerwonym prr.   ",
       "jestê     zielon¹ tra    k¹ j¹!!!!      w")
nb <- "^"

nbsp(x,nb)  #ok


## ------------------------ Zadanie 05.08 ----------------------------

## ---- Dokumentacja ----

# OPIS
# 
# Funkcja slowotwor(), dla podanego wektora liczb calkowitych x (kazda  
# liczba co do modulu mniejsza miz miliard), zwraca wektor napisow 
# zawierajacy slowny zapis tych liczb w jezyku polskim.
#
# ARGUMENTY WEJSCIOWE
# 
# x - wektor liczb calkowitych co do modulu mniejszych niz miliard
#
# WARTOSC ZWRACANA
#
# wektor napisow o dlugosci rownej dlugosci wektora x 
#

## ---- Funkcja ----

slowotwor <- function(x){
   
   stopifnot(is.numeric(x))
   stopifnot(length(x)>0)
   stopifnot(all(x<1000000000))
   stopifnot(all(x>-1000000000))
   
   n <- length(x)
   w <- character(n)  #bedziemy w nim zapisywac ostateczne wyniki dla kazdej
                      #liczby
   
   for(j in 1:n){  #przechodzimy po wszystkich liczbach w x-ie
      
      a <- as.list(unlist(stri_enc_toutf32(x[j])))  #zapisujemy liczbe w utf
      
      if(length(a)==1 & a[[1]]==48){  #osobny przypadek dla zera
         w[j] <- "zero" 
         break
      }
      
      if(a[1]==45){  #spr, czy liczba jest ujemna, jesli tak, to zapamietuje
                     #sobie gdzies ten minus
         a <- a[-1]
         min <- "minus"
      } else min <- ""
      
      aa <- unlist(stri_enc_fromutf32(a))  #dzieki temu mam te liczbe, nie 
                                           #jako calosc, ale jako 
                                           #poszczegolne cyfry
      xx <- as.numeric(aa)
      lxx <- length(xx)
      
      rx <- rev(xx)
      s <- 0
      ce <- ceiling(lxx/3)   
      l <- vector("list",ce)
      
      for(i in 1:ce){     #dziele sobie liczbe od konca na trojki
         l[[ce-i+1]] <- rx[(1+s*3):(3+s*3)]
         s <- s+1
      }
      
      
      ll <- lapply(l,rev)
      
      f <- function(i){   
         nnn <- is.na(ll[[i]])
         if(all(nnn==FALSE)==FALSE){
            ll[[i]][nnn] <- 0 
            ll[[i]] 
         } else ll[[i]]
         
      } 
      
      t <- lapply(1:ce,f)  #tam, gdzie byly NA, to zamieniam to na 0
      
      a <- c("jeden","dwa","trzy","cztery","piêæ","szeœæ","siedem","osiem",
             "dziewiêæ")
      b <- c("jedenaœcie","dwanaœcie","trzynaœcie","czternaœcie","piêtnaœcie",
             "szesnaœcie","siedemnaœcie","osiemnaœcie","dziewiêtnaœcie")
      c <- c("dziesiêæ","dwadzieœcia","trzydzieœci","czterdzieœci","piêædziesi¹t",
             "szeœædziesi¹t","siedemdziesi¹t","osiemdziesi¹t","dziewiêædziesi¹t")
      d <- c("sto","dwieœcie",stri_paste(a[3:4],"sta"),stri_paste(a[5:9],"set"))
      
      cop <- list(a,b,c,d)  
      
      q <- function(i){
         
         w1 <- if(t[[i]][2]==0){
            cop[[1]][t[[i]][3]]
         } else{
            if(t[[i]][3]==0){
               cop[[3]][t[[i]][2]]
            } else{
               if(t[[i]][2]==1){
                  cop[[2]][t[[i]][3]]
               } else{
                  stri_paste(c(cop[[3]][t[[i]][2]],cop[[1]][t[[i]][3]]),collapse=" ")
               }
            }
         }
         
         
         w2 <- if(t[[i]][1]!=0) cop[[4]][t[[i]][1]]
         
         stri_paste(c(w2,w1),collapse=" ")
         
      }
      
      z <- lapply(1:ce,q)  #kazdej trojce, uzywajac miliona if-ow :), nadaje
                           #polska nazwe
      
      tys <- c("tysi¹c","tysi¹ce","tysiêcy")
      mil <- c("milion","miliony","milionów")
      
      o <- function(i){
         k <- t[[i]]
         k[1]*100+k[2]*10+k[3]
      }
      
      licz <- lapply(1:ce,o)  #zapisuje kazda trojke jako liczbe
      
      hhh <- function(lt){
         
         if(lt==1) return(c(""))
         
         if(lt==2){
            wartys <- if(licz[[lt-1]]==1) tys[1] else{
               if(licz[[lt-1]]<=4) tys[2] else{
                  if(licz[[lt-1]]<=21) tys[3] else{
                     if(ll[[lt-1]][[3]]<=4 & ll[[lt-1]][[3]]>=2){
                        tys[2]
                        }else tys[3]
                  }
               }
            }
            return(c(wartys,""))
         }
         
         if(lt==3){
            wartys <- if(licz[[lt-1]]==1) tys[1] else{ 
               if(licz[[lt-1]]<=4) tys[2] else{ 
                  if(licz[[lt-1]]<=21) tys[3] else{ 
                     if(ll[[lt-1]][[3]]<=4 & ll[[lt-1]][[3]]>=2){
                        tys[2]
                     } else tys[3]
                  }
               }
            }
            
            warmil <- if(licz[[lt-2]]==1) mil[1] else{
               if(licz[[lt-2]]<=4) mil[2] else{ 
                  if(licz[[lt-2]]<=21) mil[3] else{
                     if(ll[[lt-2]][[3]]<=4 & ll[[lt-2]][[3]]>=2){
                        mil[2]  
                     } else mil[3]
                  }
               }
            }
            
            return(c(warmil,wartys,""))   
         }
         
      }
      
      pam <- as.list(hhh(ce))  #kolejnym milionem if-ow zapisuje, czy ma
                               #sie pojawic milion, milionow, czy miliony i 
                               #analogicznie dla tysiecy
      
      e <- function(i){
         stri_paste(c(z[[i]],pam[[i]]),collapse=" ")
      }
      
      oo <- lapply(1:ce,e)
      
      w[j] <- stri_trim_both(stri_paste(c(min,unlist(oo)),collapse=" "))
      
      #lacze wszystkie kroki, usuwam spacje i takie tam
      
   }
   
   w
   
}

## ---- Przyklady ----

slowotwor(c(45,1,-980,111,0))  #ok
slowotwor(144359)      #ok
slowotwor(4348153)     #ok
slowotwor(5523456000)  #error
slowotwor(302)         #ok


