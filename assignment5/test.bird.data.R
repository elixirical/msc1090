# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   A script to test the hypothesis that birds DONT leave from a birdfeeder in
#   random directions, using functions pulled from Circle.Utilities.R as well as
#   the EnvStats package.

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
  return(rtri(k, 0.5 * pi, 1.5 * pi, mode = pi))
}

generated.max.angle <- max.angular.diff(generate.data(num.birds))

simulated.null.hypo <- sim.null.hypo(num.birds,10000)
hist(simulated.null.hypo, breaks=21, freq=FALSE)

cdf.null.hypo <- calc.cdf(simulated.null.hypo)
#print(cdf.null.hypo)
plot(cdf.null.hypo)
#print(generated.max.angle)

p.value = 1 - calc.cumulative(cdf.null.hypo, generated.max.angle) #technically this is JUST greater than the value, not greater than or equal to but idk how to,,,,, do it yet

#print(to.tenths.ceiling(generated.max.angle))
#print(calc.cumulative(cdf.null.hypo,generated.max.angle)) #sometimes returns as numeric(0)
#print(p.value)
