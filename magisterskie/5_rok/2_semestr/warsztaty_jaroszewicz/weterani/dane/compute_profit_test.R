gifts <- read.table("gifts_test.txt", header=T)
#res_path <- "results"
res_path <- "."

names <- c()
profits <- c()
nsent <- c()
for (fname in dir(res_path)) {
  fname <- paste(res_path, fname, sep="/")
  if(grepl(".res$", fname) > 0) {
    name <- readLines(fname, n=1)
    selection <- read.table(fname, skip=1)
    gain <- sum(gifts$TARGET_D[selection$V1])
    profit <- gain - length(selection$V1) * 0.68
    #cat (c(paste('"',name,'"',sep=""), profit, length(selection$V1), "\n"))
    names <- c(names, name)
    profits <- c(profits, profit)
    nsent <- c(nsent, length(selection$V1))
  }
}
res <- data.frame(names, profits, nsent)
print (res[order(-profits),])
