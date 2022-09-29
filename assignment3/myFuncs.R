geom.pos.mean <- function(x) {
  subList <- x[(x > 0)]
  product <- prod(subList)
  numPositive <- length(subList)
  return(product**(1 / numPositive))
}

neg.mean <- function(x) {
  subList <- x[(x < 0)]
  sumOfNeg <- sum(subList)
  numNegative <- length(subList)
  return(sumOfNeg / numNegative)
}

harmonicMean <- function(x) {
  subList <- x[(x != 0)]
  inverseList <- 1 / x
  listLength <- length(inverseList)
  denominator <- sum(inverseList)
}
