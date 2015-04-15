gifts <- read.table("gifts_train.txt", header=T)

for (fname in dir()) {
  if(grepl(".res$", fname) > 0) {
    name <- readLines(fname, n=1)
    selection <- read.table(fname, skip=1)
    gain <- sum(gifts$TARGET_D[selection$V1])
    profit <- gain - length(selection$V1) * 0.68
    print (c(name, profit))
  }
}
