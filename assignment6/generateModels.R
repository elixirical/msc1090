# imports the functions from Utilities.R where all calculations are performed
source("Utilities.R")

# grabs any arguments at the end of the command
args <- commandArgs(trailingOnly = TRUE)

# these are the acceptable argument inputs
acceptable <- list(c(1:3),c(2017:2021))

# checks that the inputs are appropriate; if not, informs user of usage and stops program
if ( (length(args) != 2) ||
    !(args[1] %in% unlist(acceptable[1])) ||
    !(args[2] %in% unlist(acceptable[2])) ) {
  cat("Error: incorrect number of arguments and/or invalid arguments.\n")
  cat("Proper usage: Rscript generateModels.R X Y\n")
  cat("     Where X is is from 1-3, and Y is from 2016-2021\n")
  cat("     1: Quadratically fit first digit frequencies of data\n")
  cat("     2: Linearly fit first digit frequencies\n")
  cat("     3: Perform chi-squared goodness-of-fit test to determine\n")
  cat("        if data is significantly different from the distribution\n")
  cat("        associated with Benford's law\n")
  quit()
}

# primary driver function
main <- function(test, year) {
  frequencies <- build.digit.freqs(load.data("1710014201-eng.csv"), paste("X",year,sep=""))
  if (test == 1) {
    cat("Fitting a quadratic model.\n")
    model <- quadratic.model(frequencies)
    print(summary(model))
    plot.data.w.quad.model(frequencies, model)
  } else if (test == 2) {
    cat("Fitting a generalized linear model using a possion noise model and log link function.\n")
    model <- generalized.linear.model(frequencies)
    print(summary(model))
    plot.data.with.glm(frequencies,model)
  } else if (test == 3) {
    cat("Performing a Chi squared goodness of fit test against Benford's law.\n")
    chisq.gof <- benfords.chisq.test(frequencies)
    print(chisq.gof)
    chisq.p.value.message(chisq.gof)
  }
}

# runs the driver function
main(args[1], args[2])
