# function that loads the csv; dont hardcode the name
load.data <- function(x) {
  return(read.csv(x))
}

benfords.law <- function(x) {
  return(log10((x+1)/x))
}

benfords.law.seq <- function(x) {
  if (x > 1) {
    return( c(benfords.law.seq(x-1), benfords.law(x)) )
  } else if (x == 1) {
    return( benfords.law(1) )
  }
}

#benfords.law.df <- function() {
#  return(data.frame( digits=c(1:9), freq=benfords.law.seq(9)))
#}

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

quadratic.model <- function(my.data) {
  #return(lm(formula = counts ~ poly(digits, 2), data = my.data))
  digits_squared <- my.data$digits**2
  return(lm(formula = counts ~ digits + digits_squared, data = my.data))
}

plot.data.w.quad.model <- function(my.data, data.model) {
  #model <- lm(formula = counts ~ poly(digits, 2), data = my.data)
  plot(my.data$digits, my.data$counts)
  xx <- seq(min(my.data$digits), max(my.data$digits), len = 100)
  yy <- data.model$coef %*% rbind(1,xx,xx*xx)
  lines(xx, yy, lwd = 2, col = "red")
}

generalized.linear.model <- function(my.data) {
  glmodel <- glm(counts ~ digits, data = my.data, family = poisson(link = "log"))
  return(glmodel)
}

plot.data.with.glm <- function(my.data, data.model) {
  plot(my.data$digits, my.data$counts)
  sorted.digits <- sort(my.data$digits)
  #prediction <- predict(data.model, data.frame(counts = sorted.counts), type="response")
  #lines(sorted.counts,prediction)
  lines(sorted.digits,
        predict(data.model,
                data = data.frame(counts = sorted.digits),
                type = "response"),
        lwd = 2,
        col = "red")
}

benfords.chisq.test <- function(x) {
  return(chisq.test(x$counts, p = benfords.law.seq(9)))
}

chisq.p.value.message <- function(x) {
  if (x$p.value < 0.05) {
    cat("The null hypothesis that the data follows Benford's law is rejected, with a p value of", x$p.value, "\n")
  } else cat("The null hypothesis that the data does not follow Benford's law cannot be rejected, with a p value of", x$p.value, "\n")
}

#test <- build.digit.freqs(load.data("1710014201-eng.csv"),"X2017")
#print(test)
#test2 <- quadratic.model(test)
#print(summary(test2))
#plot.data.w.quad.model(test,test2)

#plot(test2$residuals)
#plot(test$digits,test2$residuals)
#plot(test$counts,test2$residuals)
#par(mfrow = c(1,1))
#hist(test2$residuals, breaks=11)
#qqnorm(test2$residuals)
#qqline(test2$residuals)

#test3 <- generalized.linear.model(test)
#print(summary(test3))
#plot.data.with.glm(test,test3)
#print(benfords.law.df())
#print(digit.freq.proportions(test))
#print(benfords.chisq.test(test)$p.value)

#print(benfords.chisq.test(test))
#chisq.p.value.message(benfords.chisq.test(test))
#print(chisq.test(test$counts, p=benfords.law.df()$freq))
