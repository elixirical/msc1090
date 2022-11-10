# loads Utilities.R, which contains all the functions needed
source("Utilities.R")

# grabs any trailing command line arguments
arg = commandArgs(trailingOnly = TRUE)

# one function to check the arguments, then generate a graph of the bootstrap averages + BCa intervals
driver.fn <- function(x) {
  check.args(arg)
  plot.stuff(birds(x, 1000, 2000))
}

# runs the driver function
suppressWarnings(driver.fn(arg[1]))
