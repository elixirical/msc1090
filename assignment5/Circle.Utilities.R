# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   A collection of functions that function to simulate the null hypothesis in assignment 5.
#   ie. that the direction that birds leave from a birdfeeder is uniformly random.

# Calculates the angular difference between two angles in rads
calc.angular.diff <- function(x, y) {
  if (y < x) { # checks if the second angle is less then the first; ie. if it crossed 0rad
    return(pi*2 - (x-y)) # if it does, then the difference becomes reversed
  } else return(y-x) # otherwise, the difference is the first angle subtracted from the second
}

# Takes a vector of k angles in rads corresponding to the direction birds have left the feeder
max.angular.diff <- function(x) {
  sorted.angles <- sort(x, decreasing = FALSE) # puts the angles in sequential order
  angular.differences <- c() # initializes an empty vector that will store the differences
  # beginning with the second angle in the sequential vector, calculates the angular difference
  # between that one and the preceding one, then appends it to the differences vector
  for (n in 2:length(sorted.angles)) {
    #print(angular.differences)
    angular.differences <- append(angular.differences,
                                 calc.angular.diff(sorted.angles[n-1],
                                                   sorted.angles[n]))
  }
  # repeats this step for the first and the nth angles
  angular.differences <- append(angular.differences,
                                calc.angular.diff(sorted.angles[length(sorted.angles)],
                                                  sorted.angles[1]))
  #print(angular.differences)
  return(max(angular.differences)) # returns from the vector of differences, the largest difference
}

# takes a value k and a value n, and samples k birds (ie. the angles they leave at)
# n times from a uniform distribution, and returns a vector of the max angular difference
# from each sample.
sim.null.hypo <- function(k, n) {
  n.vector <- rep(k, n) # creates a vector on n length, composed entirely of the integer k
  # for each int element of the array, samples that int many times using runif between 0
  # and 2pi rads. This generates a data frame.
  n.vector <- sapply(n.vector, runif, 0, 2*pi)
  # For each random sample, (now in the second column of a dataframe), calculates the max
  # angular difference, and returns just an array of those differences
  n.vector <- apply(n.vector, 2, max.angular.diff)
  return(n.vector) # returns that array
}

# calculates the cumulative distribution function based on an array of entries per bin
# and the total number of bins
cdf.fn <- function(x, total.points) { # x = the vector and total.points = no. of elements
  if (length(x) == 0) { return(c()) } # if the array is empty, returns an empty vector
  # if the array ISNT empty, recursively calculates the cumulative values per bin as a
  # proportion. Returns this function, which works its way down the vector X, gradually
  # adding the sum of entries in x/total points (which gives the proportion), from the end
  # of the vector to the beginning
  return( c( cdf.fn( x[-length(x)],
                     total.points),
             sum(x)/total.points))
}

# this function takes a vector of the maximum angular differences and calculates its
# distribution, then returns a dataframe of the rightmost edge of each bin in the dist,
# and the cumulative proportion per each bin, resulting in a the CDF
# (side note: i dont remember precisely why because im commenting this a week from when
# i wrote it but the builtin cumsum wouldnt work for my purposes here hence my cdf.fn)
calc.cdf <- function(n) {
  hist.results <- hist(n, breaks = 40, plot=FALSE) # generates the histogram data
  # grabs just the breaks from that data and stores it. Because we are only looking for
  # the rightmost edge of each bin, we discount the first break (reasoned because we are
  # looking fo the rightmost edge, and the hist data also generates the midpoints of each
  # bin, which we know that the first break is not the rightmost of any bin as it is not
  # sequential to any of th emidpoints listed)
  hist.breaks <- hist.results$breaks[-1]
  # generates the cdf based on the distribution and the entries per bin
  hist.cumsum <- cdf.fn(hist.results$counts,sum(hist.results$counts))
  return(data.frame(breaks = hist.breaks, Cumulative.Data = hist.cumsum))
}

# R's ceiing function doesnt seem to have a native way to round UP to the nearest tenth,
# hence this function, which works by first multiplying a number by ten, then performing
# ceil, then dividing back down. This probably only works really for tenths, not for any
# other decimal value.
to.tenths.ceiling <- function(x) {
  return(as.numeric(ceiling(x*10)/10))
}

# this function determines the value of the CDF x that the value y would fall in
calc.cumulative <- function(x, y) {
  # uses to.tenths.ceiling to find the bin that a value would fall under.
  # uses ceil because the bins are marked by their rightmost (ie. highest) edge.
  bin.to.return <- x[as.numeric(trimws(x$breaks)) == as.numeric(trimws(to.tenths.ceiling(y))), ] #trimws seems to fix the numeric(0) error
  # returns the CDF at that bin
  return(bin.to.return$Cumulative.Data)
}
