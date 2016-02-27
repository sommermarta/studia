#################################
##        PADR 2013/2014       ##
##          Wyklad  05         ##
##          2013-11-05         ##
#################################

library('stringi')

####################################################

charToRaw("aA1")
# 61 41 31

as.integer(charToRaw("aA1"))
# 97 65 49

stri_encode("aA1", "", "", to_raw=TRUE)
# [[1]] [1] 61 41 31

rawToChar(as.raw(c(97, 65, 49)))
# [1] "aA1"

stri_encode(as.raw(c(97, 65, 49)), "")
# [1] "aA1"

####################################################

stri_enc_toutf32("aФХФХКb")
# [[1]] [1]   97 1060  133 1061  155 1060  135 1061 1050   98

stri_enc_fromutf32(261)
# [1] "ą"

x <- "ala ma kota"
stri_enc_fromutf32(stri_enc_toutf32(x)[[1]][5:6])
# [1] "ma"

e <- as.raw(c(177, 230, 182))
stri_encode(e, "latin2", "")
# [1] "ąćś"
stri_encode(e, "latin1", "")
# [1] "±\032¶"

stri_length("Фhdh")    #4
stri_numbytes("Фhdh")  #5

#############################################################

stri_sub("ala ma kota", 5, 6)
# [1] "ma"

stri_sub(c("ala ma kota", "kota ma ala"), 5, 6)
# [1] "ma" " m"

stri_sub("ala ma kota", 1:5, 6)
# "ala ma" "la ma"  "a ma"   " ma"    "ma"

stri_sub(c("ala ma kota", "ala ty ala"), 5:6, 6:7)
# "ma" "y "

x <- c("ala ma kota")
stri_sub(x, 5, 6) <- "da"
x
# "ala da kota"

stri_trim_both("      if () ...;     ")
# "if () ...;"

stri_trim_left("      if () ...;     ")
# "if () ...;     "

stri_trim_right("      if () ...;     ")
# "      if () ...;"

#########################################################

paste("aaa", "bbb")
# "aaa bbb"

stri_paste("aaa", "bbb")
# "aaabbb"

stri_paste("aaa", "bbb", sep="!")
# "aaa!bbb"

stri_paste(c("aaa", "ccc"), c("bbb", "ddd"), sep="!")
# "aaa!bbb" "ccc!ddd"

stri_paste(c("aaa", "ccc"), c("bbb", "ddd"), sep="!", collapse="?")
# "aaa!bbb?ccc!ddd"

stri_dup("a", 6)
# "aaaaaa"

stri_dup("a", 1:6)
# "a"      "aa"     "aaa"    "aaaa"   "aaaaa"  "aaaaaa"

stri_dup(letters[1:3], 3)
# "aaa" "bbb" "ccc"

stri_isempty("")
# TRUE

################### statystyki w Latexu  ################################

x <- readLines("~/test.tex")
stri_stats_latex(x)
stri_stats_general(x)

#########################################################################

x <- sample(rep(LETTERS, 10),15)
x   # "Y" "I" "D" "I" "I" "J" "S" "B" "U" "C" "X" "Y" "J" "M" "U"

stri_sort(x)
# "B" "C" "D" "I" "I" "I" "J" "J" "M" "S" "U" "U" "X" "Y" "Y"

stri_order(x)
# 8 10  3  2  4  5  6 13 14  7  9 15 11  1 12

stri_trans_tolower("Ala ma Kota")
"ala ma kota"

stri_trans_toupper("Ala ma Kota")
# "ALA MA KOTA"

stri_trans_totitle("I WAS THERE OR HERE")
"I Was There Or Here"

#############################################################

stri_detect_fixed("Ala ma kota", "ma")
# TRUE

stri_detect_fixed("Ala ma kota", "MA", opts_collator=list(strength=2))
# TRUE

stri_count_fixed("aaaaa", "aa")   # 2

stri_locate_all_fixed("Ala ma kota i ma ale", "ma")
#  [[1]]
#       start end
#  [1,]     5   6
#  [2,]    15  16

stri_locate_first_fixed("Ala ma kota i ma ale", "ma")
#      start end
# [1,]     5   6

stri_locate_last_fixed("Ala ma kota i ma ale", "ma")
#      start end
# [1,]    15  16

stri_extract_all_fixed("Ala ma kota", "MA", opts_collator=list(strength=2))
# [[1]] [1] "ma"

stri_split_fixed("ala1ma1kota", "1")
# "ala"  "ma"   "kota"

#################################################################

stri_extract_all_charclass("Ala ma 123", "Ll")
# [1] "la" "ma"

stri_extract_all_charclass("Ala ma 123", "Ll", merge=FALSE)
# "l" "a" "m" "a"

stri_extract_all_charclass("Ala ma 123", "L")
# "Ala" "ma"

stri_split_charclass("Ala ma 123", "WHITESPACE")
# "Ala" "ma"  "123"

stri_extract_all_charclass("Ala ma 123", "^WHITESPACE")
# "Ala" "ma"  "123"

stri_count_charclass("ala ma kota", "Ll")  #9











































































