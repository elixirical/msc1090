source("Circle.Utilities.R")
library("EnvStats")

arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) != 1) {
  cat("Please supply a single numerical argument following the command! \n",
      "Example: Rscript test.bird.data.R 15\n")
  quit()
} else cat("Testing our hypothesis with", as.numeric(arg[1]), "birds in each batch!\n")

num.birds <- as.numeric(arg[1])

generate.data <- function(k) {
  my.data <- rtri(k, 0.5 * pi, 1.5 * pi, mode = pi)
}

#test<-c(1,2,3,2,4,1,5,6)
#hist(test)

simulated.null.hypo <- sim.null.hypo(num.birds,10000)
#print(simulated.null.hypo)
#hist(simulated.null.hypo, breaks=21, freq=FALSE)

cdf.null.hypo <- calc.cdf(simulated.null.hypo)
print(cdf.null.hypo)
