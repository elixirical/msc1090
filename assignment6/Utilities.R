# function that loads the csv
load.data <- function(x) {
  return(read.csv(x))
}

# calculates benford's law for that digit
benfords.law <- function(x) {
  return(log10((x+1)/x))
}

# generates a vector with benfords law proportions for all digits
benfords.law.seq <- function(x) {
  if (x > 1) {
    return( c(benfords.law.seq(x-1), benfords.law(x)) )
  } else if (x == 1) {
    return( benfords.law(1) )
  }
}

# function which takes above data as data frame, a year, as arguments
# then calculates the frequency of each possible first digit and returns
# the digits and the frequency indigits_squared a dataframe -> no towns with 0 pop
build.digit.freqs <- function(x, y) {
  pops.from.year <- x[y]
  no.zero.pops <- pops.from.year[pops.from.year != 0]
  no.zero.pops <- gsub(",","",no.zero.pops)
  no.zero.pops <- no.zero.pops[!is.na(suppressWarnings(as.numeric(no.zero.pops)))]
  to.return <- data.frame(digits=c(),counts=c())
  for (z in c(1:9)) {
    digit.freq <- length(no.zero.pops[substr(no.zero.pops,1,1) == toString(z)])
    to.return <- rbind(to.return, data.frame(digits=c(z),counts=c(digit.freq)))
  }
  return(to.return)
}

#digit.freq.proportions <- function(x) {
#  total <- sum(x$counts)
#  return(data.frame(digits=c(1:9),freq=x$counts/total))
#}

# generates a quadratic fit model for the first digit frequencies
quadratic.model <- function(my.data) {
  #return(lm(formula = counts ~ poly(digits, 2), data = my.data))
  digits_squared <- my.data$digits**2
  return(lm(formula = counts ~ digits + digits_squared, data = my.data))
}

# plots the data alongsite the quadratic fit
plot.data.w.quad.model <- function(my.data, data.model) {
  plot(my.data$digits, my.data$counts)
  xx <- seq(min(my.data$digits), max(my.data$digits), len = 100)
  yy <- data.model$coef %*% rbind(1,xx,xx*xx)
  lines(xx, yy, lwd = 2, col = "red")
}

# generates a linear generalized model for the first digit frequencies,
# using a poisson distribution for the noise and a log link function.
generalized.linear.model <- function(my.data) {
  glmodel <- glm(counts ~ digits, data = my.data, family = poisson(link = "log"))
  return(glmodel)
}

# plots the data alongsite the above generated LGM fit model
plot.data.with.glm <- function(my.data, data.model) {
  plot(my.data$digits, my.data$counts)
  sorted.digits <- sort(my.data$digits)
  lines(sorted.digits,
        predict(data.model,
                data = data.frame(counts = sorted.digits),
                type = "response"),
        lwd = 2,
        col = "red")
}

# performs a chi square goodness of fit test using the first
# digit frequencies against benfords law
benfords.chisq.test <- function(x) {
  return(chisq.test(x$counts, p = benfords.law.seq(9)))
}

# prints out a message depending on if the p value is greater
# or less than 0.05
chisq.p.value.message <- function(x) {
  if (x$p.value < 0.05) {
    cat("The null hypothesis that the data follows Benford's law is rejected, with a p value of", x$p.value, "\n")
  } else cat("The null hypothesis that the data follows Benford's law cannot be rejected, with a p value of", x$p.value, "\n")
}
