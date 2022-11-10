source("Utilities.R")

arg = commandArgs(trailingOnly = TRUE)

driver.fn <- function(x) {
  check.args(x)
  plot.stuff(birds(k, 1000, 2000))
}
