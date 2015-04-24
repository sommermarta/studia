library("RCurl")
library("XML")
library("dplyr")
library("stringi")
library("rvest")

# znalezc polskie slowa z 'double t'

polaczenie <- file("C:\\Users\\Marta\\Desktop\\dla_taty.txt", "a")

for(i in 1:3933){

   link <- html(stri_paste("http://sjp.pl/slownik/lp.phtml?page=", i, collapse=""))
   
   slowa <- html_nodes(link, "td a")
   slowa2 <- html_text(slowa)
   slowa2 <- tryCatch(repair_encoding(slowa2), error=function(condition) slowa2)
   
   czy_dobre <- html_nodes(link, "td:nth-child(2)")
   czy_dobre2 <- html_text(czy_dobre)[-1]
   
   words <- slowa2[which(czy_dobre2=="tak")]  
   words_tt <- words[stri_detect_fixed(words, "tt")]
   
   if(length(words_tt) > 0) write.table(words_tt, polaczenie, append=TRUE, 
                                        row.names=FALSE, col.names=FALSE, quote=FALSE)
   print(i)
   
}

close(polaczenie)

# sortowanie:

slownik <- readLines("C:\\Users\\Marta\\Desktop\\dla_taty.txt")

dane <- data.frame(slownik)
dane$ileliter <- nchar(slownik)

posortowane_dane <- dane %>%
  arrange(ileliter) %>%
  group_by(ileliter) %>%
  arrange(slownik) 

polaczenie <- file("C:\\Users\\Marta\\Desktop\\posortowane_dla_taty.txt")
write.table(posortowane_dane$slownik, polaczenie, append=FALSE, 
            row.names=FALSE, col.names=FALSE, quote=FALSE)

