# function that loads the csv; dont hardcode the name
load.data <- function(x) {
  return(read.csv(x))
}

# function which takes above data as data frame, a year, as arguments
# then calculates the frequency of each possible first digit and returns
# the digits and the frequency in a dataframe -> no towns with 0 pop
build.digit.freqs <- function(x, y) {
  pops.from.year <- x[y]
  no.zero.pops <- pops.from.year[pops.from.year != 0]
  no.zero.pops <- gsub(",","",no.zero.pops)
  no.zero.pops <- no.zero.pops[!is.na(as.numeric(no.zero.pops))]
  to.return <- data.frame(digits=c(),counts=c())
  print(to.return)
  for (z in c("1","2","3","4","5","6","7","8","9")) {
    digit.freq <- length(no.zero.pops[substr(no.zero.pops,1,1) == z])
    to.return <- rbind(to.return, data.frame(digits=c(z),counts=c(digit.freq)))
  }
  return(to.return)
}

#test <- build.digit.freqs(load.data("1710014201-eng.csv"),"X2018")
#print(test)
