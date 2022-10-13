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
