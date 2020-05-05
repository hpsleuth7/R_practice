# Reads CSV files and prints (outputs?)  mean of each row
# Usage: Rscript readings.R <action flag> [filenames]
#        If no filenames passed, reads from standard input 

main <- function () {
  args <- commandArgs(trailingOnly=TRUE)
  action <- args[1]
  fnames <- args[-1]
  
  # check if valid action flag given
  # stopifnot(action %in% c("--min","--max","--mean"))
  if (!action %in% c("--min","--max","--mean")) {
  	return("Invalid flag")
  }

  # pass either files or stdinput to analyze function
  if (length(fnames)==0) {
    analyze(file("stdin"),action)
  } else {
  	for (f in fnames) {
  	  analyze(f,action)
    }
  }
}

analyze <- function(fname, action) {
# read fname as csv and analyze table according to action flag
# print results
  dat <- read.csv(file=fname,header=FALSE)
  if (action == "--min") {
    values <- apply(dat,1,min)
  } else if (action=="--max") {
    values <- apply(dat,1,max)
  } else if (action=="--mean") {
    values <- apply(dat,1,mean)
}
cat(values,sep="\n") 
}

main()
