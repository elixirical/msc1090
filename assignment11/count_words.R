source("myutils.R")

args = commandArgs(trailingOnly = TRUE)

driver <- function(x) {
  if (check.args(args) == 2) {
    results <- word_count(read_file(x[1]), format.stop.words(read_file(x[2])))
  } else results <- word_count(read_file(x[1]))
  plotTopWords(results)
}

driver(args)
