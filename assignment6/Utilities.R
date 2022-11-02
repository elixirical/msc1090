# function that loads the csv; dont hardcode the name
load.data <- function(x) {
  return(read.csv(x))
}

# function which takes above data as data frame, a year, as arguments
# then calculates the frequency of each possible first digit and returns
# the digits and the frequency in a dataframe -> no towns with 0 pop
build.digit.freqs <- function(x, y) {
  pops.from.year <- x[y]
  no.zero.pops <- pops.from.year[pops.from.year != 0]
  no.zero.pops <- gsub(",","",no.zero.pops)
  no.zero.pops <- no.zero.pops[!is.na(as.numeric(no.zero.pops))]
  to.return <- data.frame(digits=c(),counts=c())
  for (z in c(1:9)) {
    digit.freq <- length(no.zero.pops[substr(no.zero.pops,1,1) == toString(z)])
    to.return <- rbind(to.return, data.frame(digits=c(z),counts=c(digit.freq)))
  }
  return(to.return)
}

quadratic.model <- function(my.data) {
  return(lm(formula = counts ~ poly(digits, 2), data = my.data))
}

plot.data.w.quad.model <- function(my.data, data.model) {
  #model <- lm(formula = counts ~ poly(digits, 2), data = my.data)
  plot(my.data$digits, my.data$counts)
  xx <- seq(min(my.data$digits), max(my.data$counts), len = 100)
  yy <- data.model$coef %*% rbind(1,xx,xx*xx)
  lines(xx, yy, lwd = 2, col = "red")
}

test <- build.digit.freqs(load.data("1710014201-eng.csv"),"X2017")
print(test)
test2 <- quadratic.model(test)
print(summary(test2))
plot.data.w.quad.model(test,test2)
