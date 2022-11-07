source("Circle.Utilities.R")
library(boot)

#diffs <- sim.null.hypo(5,1000)
#print(diffs)

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

birds <- function(k, m, n) { # three integers
  bootstrap.t0.bca <- data.frame(k=c(), t=c(), upper=c(), lower=c())
  for (z in 5:k) {
    #print(z)
    distribution <- sim.null.hypo(z, m)
    bootstrap <- non.para.bootstrapping(distribution, n)
    #print(summary(bootstrap))
    bootstrap.bca.ci <- boot.ci(bootstrap)$bca[4:5]
    bootstrap.t0.bca <- rbind(bootstrap.t0.bca, data.frame(k=c(z), t=c(bootstrap$t0), upper=c(bootstrap.bca.ci[1]), lower=c(bootstrap.bca.ci[2])))
    #bootstrap.t0.bca[[z]] <- list(bootstrap$t0, bootstrap.bca.ci)
  }
  return(bootstrap.t0.bca)
}

plot.stuff <- function(x) {

}

#test <- non.para.bootstrapping(diffs, 2000)

#print(test)
#print(test$t0)
#test2 <- boot.ci(test, type="bca")
#print(test2)
#print(test2$bca[4:5])

print(birds(2, 1000, 1000)) #num 2 must be smaller than num 3;;;; diagnose !
#print(sim.null.hypo(1,10))
