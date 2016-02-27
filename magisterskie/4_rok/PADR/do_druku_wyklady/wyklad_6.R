#################################
##        PADR 2013/2014       ##
##          Wyklad  06         ##
##          2013-11-12         ##
#################################


#### WYRAZENIA REGULARNE (REGULAR EXPRESSION, REGEX)
#### DATA I CZAS W R

require('stringi')

stri_extract_all_regex("abc 123 ABC ?????? ??????? !@#.", ".")
#znajduje dowolny znak

stri_extract_all_fixed("abc 123 ABC ?????? ??????? !@#..", ".")
#znajduje wszystkie kropki (tym sie rozni fixed od regex)

stri_extract_all_regex("abc 123 aABC ?????? ??????? !@#.", "a.")
#znajduje a, a potem cos dowolnego - rozroznia wielkosc liter

stri_extract_all_regex("abc aba bab aaa bbb babc", "a.")
# "ab" "ab" "a " "ab" "aa" "a " "ab"

stri_extract_all_regex("abc aba bab aaa bbb badc", "a.c")
# "abc" "adc"

stri_extract_all_regex("abc aba bab a.c bbb badc", "a.c")
# "abc" "a.c" "adc"

stri_extract_all_regex("abc aba bab a.c bbb badc", "a\\.c")
# "a.c"

stri_extract_all_regex("a\nc aac", "a.c")
# "aac" -> nie dopasowuje znaku nowej linii!!!

stri_extract_all_regex("a\\a  ada aba", "a.a")
# "a\\a" "ada"  "aba"

stri_extract_all_regex("a\\a", "a..a")
# NA

stri_extract_all_regex("a\\a", "a\\a")
# NA

stri_extract_all_regex("a\\a  ada aba", "a\\\\a")
# "a\\a"

cat("a\a")  # a
cat("a\\a")  # a\a
cat("a\\\\a")  # a\\a

stri_extract_all_regex("abc abd aaa bbb", c("a..", "b.."))
# [[1]] [1] "abc" "abd" "aaa" [[2]] [1] "bc " "bd " "bbb"

stri_extract_all_regex(c("abc abd aaa bbb", "bbb aaa"), c("a..", "b.."))
# [[1]] [1] "abc" "abd" "aaa" [[2]] [1] "bbb"

stri_extract_all_regex(c("abc abd aaa bbb", "bbb aaa"), "b..")
# [[1]] [1] "bc " "bd " "bbb" [[2]] [1] "bbb"

stri_extract_all_regex("aba aca ada aea", "a[bcd]a")  
# "aba" "aca" "ada" -> to co w nawiasie to jest "lub"

stri_extract_all_regex("a.a aca ada aea", "a[.]a")
# "a.a"

stri_extract_all_regex("abc abd aaa bbb", "[ab]b[cb]")
# "abc" "bbb"

stri_extract_all_regex("aba aca ada aea a aa,a", "a[^e]a")
# "aba" "aca" "ada" "a a" "a,a" -> dopelnienie (wszystko, tylko nie e)

stri_extract_all_regex("aba aca ada aea a??a", "a[a-z]a")
# "aba" "aca" "ada" "aea" -> od a do z, uwaga!!! utf8 !!!

stri_extract_all_regex("aba aca ada aea a??a", "a[a-z?]a")
# "aba" "aca" "ada" "aea"

stri_extract_all_regex("aba aca adca aea a??a", "a[a-z?][a-z?]a")
# "adca" "a??a"

stri_extract_all_regex("aba aca ada aea aXa", "a[a-zA-Z]a")
# "aba" "aca" "ada" "aea" "aXa" -> duze i male

stri_extract_all_regex("aba aca ada aea aXa", "a[[a-z][A-Z]]a")
# "aba" "aca" "ada" "aea" "aXa" -> zeby bylo czytelniej

stri_extract_all_regex("aba aca ada aeRa aXa", "a[a-z][A-Z]a")
# "aeRa" -> to nie to samo, co powyzsze!!!

stri_extract_all_regex("??", "[??-??]")
# "?" "?"

stri_extract_all_regex("ala a^a", "a\\^a")
# "a^a"

stri_extract_all_regex("ala a^a", "a[\\^]a")
# "a^a"

stri_extract_all_regex("(a) [a] {a}", "[\\[({]a[\\])}]")
# "(a)" "[a]" "{a}"

stri_extract_all_regex("????^d:]", ":]")
# ":]" -> jesli nawias nie byl wczesniej otwarty, to zalapie o co chodzi

stri_extract_all_regex("????^d:]", ":\\]")
# ":]"

stri_extract_all_regex("????^d[:", "[:")
# error -> niedomkniety nawias

stri_extract_all_regex("????^d[:", "\\[:")
# "[:" -> tu juz zalapie, o co chodzi

stri_extract_all_regex("gry???? S??JKA", "\\p{L}")
# "g" "r" "y" "S" "J" "K" "A" -> wyciaga wszystkie litery

stri_extract_all_charclass("gry???? S??JKA", "L", merge=FALSE)
# "g" "r" "y" "S" "J" "K" "A" -> to samo na charclassie

stri_extract_all_charclass("gry???? S??JKA", "L", merge=TRUE)
# "gry" "S"   "JKA"

stri_extract_all_regex("grąy???? S??JKA", "\\p{Ll}")
# "g" "r" "ą" "y" -> wyciaga male litery (rozumie tez polskie znaki)

stri_extract_all_regex("gry???? S??JKA", "[\\p{Ll}\\p{Lu}]")
# "g" "r" "y" "S" "J" "K" "A" -> male lub duze litery

stri_extract_all_regex("gry???? S??JKA 123", "[\\p{Ll}\\p{Nd}]")
# "g" "r" "y" "1" "2" "3" -> male lub cyfry

stri_extract_all_regex("gry? S??JKA 123", "[^\\p{Ll}]")
# "?" " " "S" "?" "?" "J" "K" "A" " " "1" "2" "3"

stri_extract_all_regex("gry? S??JKA 123", "\\P{Ll}")
# to samo -> wszystko oprócz małych (duże P to dopełnienie)

ru <- "Я, как и вы в моем городе и самый"
stri_extract_all_regex(ru, "\\p{Ll}")
# rozumie cyrylice

znaki <- stri_enc_fromutf32(32:126)
znaki  
# " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]
#   ^_`abcdefghijklmnopqrstuvwxyz{|}~ "

stri_extract_all_regex(znaki, "\\p{P}")
# "!"  "\"" "#"  "%"  "&"  "'"  "("  ")"  "*"  ","  "-"  "."  "/"  ":"  ";"  
# "?"  "@" "["  "\\" "]"  "_"  "{"  "}" -> szuka znakow

znaki2 <- stri_enc_fromutf32(32:10240)
stri_extract_all_regex(znaki2, "\\p{Ps}")
# szuka nawiasow (jakis roznych dziwnych tez)

stri_extract_all_regex("ala ma\tkota\noj", "\\p{Zs}")
#  " " -> biale znaki, ale nie nowa linia i tabulator

stri_extract_all_regex("ala ma\tkota\noj", "[\\p{Zs}\\t\\n]")
#  " "  "\t"  "\n"

stri_extract_all_regex("ala ma\tkota\noj", "\\s") 
# " "  "\t"  "\n" -> spacje

stri_extract_all_regex("ala ma\tkota\noj,6432.35fs", "\\w")      
# "a" "l" "a" "m" "a" "k" "o" "t" "a" "o" "j" "6" "4" "3" "2" "3" "5" "f" 
# "s" -> slowa, pomija biale znaki i znaki przystankowe

stri_extract_all_regex("ala ma\tkota\noj,6432.35fs", "\\W")    
# " "  "\t"  "\n" ","  "." -> duze W, czyli zaprzeczenie 

stri_extract_all_regex("ala ma\tkota\noj,6432.35fs", "\\d")
# "6" "4" "3" "2" "3" "5" -> digit, czyli cyfry

stri_extract_all_regex("ala ma\tkota\noj,6432.35fs", "\\D")
# zaprzeczenie digita

stri_extract_all_regex("ala ma\tkota\noj", "\\t")
# szuka znakow \t

stri_extract_all_regex("ala ma\tkota\noj", "\\n")
# szuka znakow nowej linii

stri_extract_all_regex("ala ma\nala da\nkasia nie ma", "^a.....")
# "ala ma" -> szuka slow zaczynajzcych sie na a tylko na poczatku wyrazenia

stri_extract_all_regex("ala ma\nala da\nkasia nie ma", "(?m)^a.....") 
# "ala ma" "ala da" -> bierze tez pod uwage znak nowj linii
       
stri_extract_all_regex("ala ma\nala ma da\nkasia nie ma", "....ma$") 
# "nie ma" -> szuka, co ma na koncu "ma"

stri_extract_all_regex("ala ma\nala ma da\nkasia nie ma", "(?m)....ma$")
# "ala ma" "nie ma" -> znowu multiline (czyli uwzglednia tez \n)

stri_extract_all_regex(c("a", "aa", "aaa", "aaaa"), "...") 
# [[1]] [1] NA [[2]] [1] NA [[3]] [1] "aaa" [[4]] [1] "aaa"
# dopasowuje trzyliterowe slowa

stri_extract_all_regex(c("a", "aa", "aaa", "aaaa"), "^...$")
# [[1]] [1] NA [[2]] [1] NA [[3]] [1] "aaa" [[4]] [1] NA
# dokladnie trzyliterowe

stri_extract_all_regex(c("aaa", "aaaa"), "^...$(?# to jest komentarz)")       
# [[1]] [1] "aaa" [[2]] [1] NA -> ?# to komentarz

stri_extract_all_regex(c("a", "aaA", "aaa", "AAA"), "(?i)^aaa$")       
# [[1]] [1] NA [[2]] [1] "aaA" [[3]] [1] "aaa" [[4]] [1] "AAA"
# ?i -> ignoruje wielkosc liter

stri_extract_all_regex(c("a", "aa", "aaa", "aaaa"), "^(.|..)$")       
# [[1]] [1] "a" [[2]] [1] "aa" [[3]] [1] NA [[4]] [1] NA
# jedno lub dwuwyrazowe

stri_extract_all_regex(c("a", "aa", "aaa", "aaaa"), "a*")     
# * - 0 lub wiecej razy

stri_extract_all_regex("aabaaa", "a+")   
# [1] "aa"  "aaa" -> jeden lub wiecej razy

stri_extract_all_regex("cacaabaaa", "a{2,}")        
# "aa"  "aaa" -> dwa lub wiecej razy

stri_extract_all_regex("aabaaabaaaa", "a{2,4}")        
# "aa"   "aaa"  "aaaa" -> od dwoch do czterech razy

stri_extract_all_regex("aabaaabaaaa", "(aa)+") 
# "aa"   "aa"   "aaaa" -> wybierze parzyste

stri_extract_all_regex("hell hello hell shell hell", "hell")
# "hell" "hell" "hell" "hell" "hell"

stri_extract_all_regex("hell hello.hell shell hell", "\\Whell\\W")
# ".hell " -> tylko jedno (bo najpierw nie wyraz potem hell i potem znow)

stri_extract_all_regex("hell hello hell shell hell", "(\\W|^)hell(\\W|$)")
# "hell "  " hell " " hell" 

stri_extract_all_regex("aabaaabaaaa", "a{0,1}")        
stri_extract_all_regex("aabaaabaaaa", "a?")        

stri_extract_all_regex("mam: 'aaa', 'bbb', 'cc', 'd'","'.*'")
# "'aaa', 'bbb', 'cc', 'd'" -> dzialaja zachlannie -> dopasowuja tak duzo,
#                              jak sie da

stri_extract_all_regex("mam: 'aaa', 'bbb', 'cc', 'd'","'.*?'")
# "'aaa'" "'bbb'" "'cc'"  "'d'" -> niezachlannie = jak najmniej sie da - ?

stri_extract_all_regex("mam: 'aaa', 'bbb', 'cc', 'd'","'[^']*'")
# powyzsze innym sposobem

stri_extract_all_regex("mam: 'aaa', 'bbb', 'cc', 'd'","'(.*?)'")
# to samo co bez nawiasow

stri_match_all_regex("mam: 'aaa', 'bbb', 'cc', 'd'","'(.*?)'")
# ale funkcja match dziala juz inaczej -> pierwsza kolumna tak jakby bez 
# nawiasow, a druga to to, co w nawiasie
# [[1]]
#       [,1]    [,2] 
# [1,] "'aaa'" "aaa"
# [2,] "'bbb'" "bbb"
# [3,] "'cc'"  "cc" 
# [4,] "'d'"   "d" 


stri_match_all_regex("12.3, 12, 3.1415, -2145.2, 532, -124","\\d+\\.\\d+")
# wyluskaj wszystkie ulamki
# [[1]]
#       [,1]    
# [1,] "12.3"  
# [2,] "3.1415"
# [3,] "2145.2"

stri_match_all_regex("12.3, 12, 3.15, -2145.2, 532, -124","(\\d+)\\.(\\d+)")
# [[1]]
#       [,1]     [,2]   [,3]
# [1,] "12.3"   "12"   "3" 
# [2,] "3.15"   "3"    "15"
# [3,] "2145.2" "2145" "2" 


stri_extract_all_regex("12.3, 12, 3.1415, -2145.2, 532, -124",
                       "-?(\\d+)(\\.(\\d+))?")
# wszystkie liczby: czy minus - cyfry jak dlugo sie da - jesli kropka to te
# czesc ulamkowa tez

stri_match_all_regex("12.3, 12, 3.1415, -2145.2, 532, -124",
                     "-?(\\d+)(\\.(\\d+))?")
# [[1]]
#      [,1]      [,2]   [,3]    [,4]  
# [1,] "12.3"    "12"   ".3"    "3"   
# [2,] "12"      "12"   NA      NA    
# [3,] "3.1415"  "3"    ".1415" "1415"
# [4,] "-2145.2" "2145" ".2"    "2"   
# [5,] "532"     "532"  NA      NA    
# [6,] "-124"    "124"  NA      NA   


stri_match_all_regex("12.3, 12, 3.1415, -2145.2, 532, -124",
                     "-?(\\d+)(?:\\.(\\d+))?")
# nie pokazuj w macierzy tej czesci ulamkowej z kropka - ?:


x <- "\\begin{enumerate}\\begin{itemize}....\\end{itemize}\\end{enumerate}
      \\end{figure}"
stri_extract_all_regex(x,"\\\\begin\\{([a-z]+)\\}.*?\\\\end\\{\\1\\}")
# "\\begin{enumerate}\\begin{itemize}....\\end{itemize}\\end{enumerate}"

stri_match_all_regex(x,"\\\\begin\\{([a-z]+)\\}(.*?)\\\\end\\{\\1\\}")
# [,1]"\\begin{enumerate}\\begin{itemize}....\\end{itemize}\\end{enumerate}"
# [,2] "enumerate"
# [,3] "\\begin{itemize}....\\end{itemize}"

stri_replace_all_regex("sala ama", "a(.)a", "$0$0")
# "salaala amaama"

stri_replace_all_regex("ala ama", "a(.)a", "$0$0$1$1$1")
# "alaalalll amaamammm"


###### DATA I CZAS W R

d <- Sys.Date() # "2013-11-15" -> data dzisiejsza
as.Date("1970-01-01")
unclass(as.Date("1970-01-01")) # 0 - pierwszy dzien epoki unixa

as.Date("1970/01/01")   # "1970-01-01"   
as.Date("1970")     # error

c <- Sys.time()  # "2013-11-15 15:16:36 CET"

d + 10  # co jest za 10 dni -> "2013-11-25"
d - 31  # "2013-10-15" -> miesiac temu

difftime(d, d-10) # Time difference of 10 days

strftime(Sys.time(), "%a")  # "Pt"
strftime(Sys.time(), "%A")  # "piatek"

strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")   # "2013-11-15 15:20:21"    
strftime(Sys.time(), "%A, %d %B %Y")      # "piątek, 15 listopad 2013"     
strptime("12 listopad 2013", "%d %B %Y") # "2013-11-12

