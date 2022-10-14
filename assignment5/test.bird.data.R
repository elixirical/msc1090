source("Circle.Utilities.R")


arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) != 0) {
  cat("Please supply a single numerical argument following the command! \n",
      "Example: Rscript test.bird.data.R 15")
  quit()
} else cat("Testing our hypothesis with", arg[1], " birds in each batch!")

my.data <- rtri(arg[1], 0.5 * pi, 1.5 * pi, mode = pi)
