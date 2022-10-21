# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   A script to test the hypothesis that birds DONT leave from a birdfeeder in
#   random directions, using functions pulled from Circle.Utilities.R as well as
#   the EnvStats package.

# imports functions defined in Circle.Utilities.R
source("Circle.Utilities.R")
# Imports the EnvStats package, while supprseeing annoying messages
suppressPackageStartupMessages(library("EnvStats", quietly=TRUE))

arg <- commandArgs(trailingOnly = TRUE)
if ((length(arg) != 1) || is.na(as.numeric(arg[1]))) {
  cat("Please supply a single numerical argument following the command! \n",
      "Example: Rscript test.bird.data.R 15\n")
  quit()
} else cat("Testing our hypothesis with", as.numeric(arg[1]), "birds in each batch!\n")

# stores the given argument in a nice easy to remember variable
num.birds <- as.numeric(arg[1])

# generates artifician H1 distribution to test against the null
generate.H1.data <- function(k) {
  return(rtri(k, 0.5 * pi, 1.5 * pi, mode = pi))
}

# generates the maximum angular difference in the H1 data
generate.H1.max.angle <- function(H1.data) {
  return(max.angular.diff(generate.H1.data(H1.data)))
}

# grabs datapoints from between 0 and 2pi from a uniform distribution
simulate.H0 <- function(k) {
  return(sim.null.hypo(k,10000))
  #hist(simulated.null.hypo, breaks=21, freq=FALSE)
}

# generates the CDF for the null distribution
gen.H0.cdf <- function(x) {
  return(calc.cdf(x))
  #plot(cdf.null.hypo)
}

# compares the H1 max angle against the null CDF to determine the p value for that angle
calc.p.value <- function(cdf.null.hypo, generated.max.angle) {
  return(1 - calc.cumulative(cdf.null.hypo, generated.max.angle))
  #^technically this is JUST greater than the value, not greater than or equal to but idk how to,,,,, do it yet while still using calc cumulative
}

# spits out text depending on if the p value is significant against a 0.05 confidence rating
determine.significance <- function(p) {
  if (p < 0.05) {
    cat("The test is significant, with a p value of", p, "\n")
  } else cat("The test is not significant, with a p value of", p, "\n")
}

# chains together all the above functions
test.bird.data.fn <- function() {
  H1.max.angle <- generate.H1.max.angle(generate.H1.data(num.birds))
  H0.cdf <- gen.H0.cdf(simulate.H0(num.birds))
  p.value <- calc.p.value(H0.cdf,H1.max.angle)
  determine.significance(p.value)
}

# actually runs all the functions
test.bird.data.fn()
