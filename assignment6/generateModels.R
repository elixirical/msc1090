args <- commandArgs(trailingOnly = TRUE)
acceptable <- data.frame(one=c(1:3),
                             two=c(2016:2021)
if ( (length(args) != 2) ||
    !(args[1] %in% acceptable$one) ||
    !(args[2] %in% acceptable$two) ) {
  cat("Error: incorrect number of arguments and/or invalid arguments.\n")
  cat("Proper usage: Rscript generateModels.R X Y\n")
  cat("     Where X is is from 1-3, and Y is from 2016-2021\n")
  cat("     1: Quadratically fit first digit frequencies of data\n")
  cat("     2: Linearly fit first digit frequencies\n")
  cat("     3: Perform chi-squared goodness-of-fit test to determine\n")
  cat("        if data is significantly different from the distribution\n")
  cat("        associated with Benford's law\n")
}
