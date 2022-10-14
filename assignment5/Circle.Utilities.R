calc.angular.diff <- function(x, y) {
  if (y < x) {
    return(pi*2 - (x-y))
  } else return(y-x)
}

max.angular.diff <- function(x) {
  sorted.angles <- sort(x, decreasing = FALSE)
  angular.differences <- c()
  for (n in 2:length(sorted.angles)) {
    #print(angular.differences)
    angular.differences <- append(angular.differences,
                                 calc.angular.diff(sorted.angles[n-1],
                                                   sorted.angles[n]))
  }
  angular.differences <- append(angular.differences,
                                calc.angular.diff(sorted.angles[length(sorted.angles)],
                                                  sorted.angles[1]))
  #print(angular.differences)
  return(max(angular.differences))
}

sim.null.hypo <- function(k, n) {
  n.vector <- rep(k, n)
  n.vector <- sapply(n.vector, runif, 0, 2*pi)
  n.vector <- apply(n.vector, 2, max.angular.diff)
  return(n.vector)
}

cdf.fn <- function(x, total.points) { #vector of counts per bin
  if (length(x) == 0) { return(c()) }
  return( c( cdf.fn( x[-length(x)],
                     total.points),
             sum(x)/total.points))
}

calc.cdf <- function(n) {
  hist.results <- hist(n, breaks = 40, plot=FALSE)
  hist.breaks <- hist.results$breaks[-1]
  hist.cumsum <- cdf.fn(hist.results$counts,sum(hist.results$counts))
  return(data.frame(breaks = hist.breaks, Cumulative.Data = hist.cumsum))
}

to.tenths.ceiling <- function(x) {
  return(ceiling(x*10)/10)
}

calc.cumulative <- function(x, y) {
  bin.to.return <- x[x$breaks == to.tenths.ceiling(y), ]
  return(bin.to.return$Cumulative.Data)
}
