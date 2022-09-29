# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   Various functions that calculate the geometric mean of positive numbers,
#   the arithmetic men of negative numbers, and the harmonic means of all
#   non-0 numbers.

geom.pos.mean <- function(x) {
  subList <- x[(x > 0)]             #creates a vector composed of values that are greater than 0
  product <- prod(subList)          #calculates the product of all the + values
  numPositive <- length(subList)    #grabs the count of all + values
  return(product**(1 / numPositive))#returns the geometric positive mean
}

neg.mean <- function(x) {
  subList <- x[(x < 0)]             #creates a vector composed of values less than 0
  sumOfNeg <- sum(subList)          #calculates the sum of all - numbers
  numNegative <- length(subList)    #grabs the count of all - numbers
  return(sumOfNeg / numNegative)    #returns the mean of all - numbers
}

harmonicMean <- function(x) {
  subList <- x[(x != 0)]            #creates a vector composed of values that are not 0
  inverseList <- 1 / subList        #creates a vector composed of the inverse values of subList
  listLength <- length(inverseList) #grabs the count of all non-0 values
  denominator <- sum(inverseList)   #sums up the inverse values
  return (listLength / denominator) #divides the count by the sum of the inverse values
}
