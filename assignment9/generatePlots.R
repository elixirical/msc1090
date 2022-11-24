# loads Utilities.R, which contains all the functions needed
source("plottingTools.R")

# grabs any trailing command line arguments
arg = commandArgs(trailingOnly = TRUE)

# one function that checks then runs the main script
driver.fn <- function(x) {
  check.args(x)
  primary.driver(x)
}

# runs the above function
suppressWarnings(driver.fn(arg))