source("Circle.Utilities.R")
library(boot)

diffs <- sim.null.hypo(5,1000)

calculate.mode <- function(x) {
  hist.of.vector <- hist(x, breaks=40, plot = FALSE)
  largest.density <- sort(hist.of.vector$density, decreasing = TRUE)[1]
  index.of.largest.density <- match(largest.density,hist.of.vector$density)
  return(hist.of.vector$mids[index.of.largest.density])
}

my.mode <- function(my.data, i) {
  return(calculate.mode(my.data[i]))
}

non.para.bootstrapping <- function(x, n) {
  return(boot(data = x, statistic = my.mode, R = n))
}



test <- non.para.bootstrapping(diffs, 2000)

print(test)
print(boot.ci(test))
