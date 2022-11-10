source("Circle.Utilities.R")
library(boot)
library(plotrix)

# checks to make sure that a single numeric argument is passed to the command
check.args <- function(x) {
  if ((length(x) != 1) || is.na(as.numeric(x[1]))) {
    cat("Please supply a single numeric value k following the scriptname.")
    cat("Example: Rscript Driver.R 5")
    quit()
  }
}

# calculates the mode of a given vector of numbers
calculate.mode <- function(x) {
  hist.of.vector <- hist(x, breaks=40, plot = FALSE) # generates a histogram using the vector
  largest.density <- sort(hist.of.vector$density, decreasing = TRUE)[1] # creates a sorted list of the greatist density, and grabs the first (ie. largest) value
  index.of.largest.density <- match(largest.density,hist.of.vector$density) #matches the largest density with the original sequence to find its index
  return(hist.of.vector$mids[index.of.largest.density]) # returns the mid point at that index
}

# required for the non parametric boostrapping function below
my.mode <- function(my.data, i) {
  return(calculate.mode(my.data[i]))
}

# performs non parametric boostrapping using the boot library
non.para.bootstrapping <- function(x, n) {
  return(boot(data = x, statistic = my.mode, R = n))
}

# ``
birds <- function(k, m, n) { # three integers
  bootstrap.t0.bca <- data.frame(k=c(), t=c(), lower=c(), upper=c())
  for (z in 5:k) {
    #print(z)
    distribution <- sim.null.hypo(z, m)
    bootstrap <- non.para.bootstrapping(distribution, n)
    #print(summary(bootstrap))
    bootstrap.bca.ci <- boot.ci(bootstrap)$bca[4:5]
    bootstrap.t0.bca <- rbind(bootstrap.t0.bca, data.frame(k=c(z), t=c(bootstrap$t0), lower=c(bootstrap.bca.ci[1]), upper=c(bootstrap.bca.ci[2])))
    #bootstrap.t0.bca[[z]] <- list(bootstrap$t0, bootstrap.bca.ci)
  }
  return(bootstrap.t0.bca)
}

plot.stuff <- function(x) {
  plotCI(x$k, x$t, x$upper-x$t, x$t-x$lower)
}

#test <- birds(2, 1000, 2000) #num 2 must be smaller or equal than num 3;;;; diagnose !
#print(test)
#plot.stuff(test)
