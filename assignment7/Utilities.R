source("Circle.Utilities.R")
library(boot)
library(plotrix)

# checks to make sure that a single numeric argument is passed to the command
check.args <- function(x) {
  if ((length(x) != 1) || is.na(as.numeric(x[1]))) {
    cat("Please supply a single numeric value k following the scriptname.\n")
    cat("Example: Rscript Driver.R 5\n")
    quit()
  }
}

# calculates the mode of a given vector of numbers
calculate.mode <- function(x) {
  hist.of.vector <- hist(x, breaks=51, plot = FALSE) # generates a histogram using the vector
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

# this function takes in functions from Circle.Utilities.R and uses them to generate the max angular
# distributions for samples from
birds <- function(k, m, n) { # k = number of birds per sample, m = number of times to sample, n = boostraps
  bootstrap.t.bca <- data.frame(k=c(), t=c(), lower=c(), upper=c()) #inits a data frame for storing t mean + lower and upper CIs
  for (z in 2:k) { #for each valid possible number of birds per sample, up to k...
    distribution <- sim.null.hypo(z, m) # generate the max angular distribution
    bootstrap <- non.para.bootstrapping(distribution, n) # run boostraps on this distribution
    bootstrap.bca.ci <- boot.ci(bootstrap)$bca[4:5] # calculate the BCa intervals
    bootstrap.t.bca <- rbind(bootstrap.t.bca, data.frame(k=c(z), t=c(mean(bootstrap$t)), lower=c(bootstrap.bca.ci[1]), upper=c(bootstrap.bca.ci[2]))) #adds to data frame
  }
  print(bootstrap.t.bca)
  return(bootstrap.t.bca)
}

# plots the distribution using plotrix
plot.stuff <- function(x) {
  plotCI(x$k, x$t, x$upper-x$t, x$t-x$lower)
}

#test <- birds(2, 1000, 2000) #num 2 must be smaller or equal than num 3;;;; diagnose !
#print(test)
#plot.stuff(test)
