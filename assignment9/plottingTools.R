library(ggplot2)

# imports a file
load.file <- function(filename) {
  return(read.csv(filename))
}

#get.means <- function(column) {
#  return(mean(column))
#  return(tryCatch({
#      mean(column)
#    },
#    error = function(e) {
#      return(NA)
#    }
#  ))
#}

#get.sd <- function() {
#  return(sd(column))
#}

data.summary <- function(rawdata) {

  injury.types <- unique(rawdata$injury)
  print(injury.types)

  temp <- data.frame(group=c(),
                     mean.cell1=c(), sd.cell1=c(),
                     mean.cov1=c(),  sd.cov1=c(),
                     mean.cell2=c(), sd.cell2=c(),
                     mean.cov2=c(),  sd.cov2=c())
  #temp <- data.frame(group=c())

  for (x in injury.types) {
    subset <- rawdata[rawdata$injury == x, ]

    mean1.cell <- mean(subset$layer1.iba.count)
    sd1.cell <- sd(subset$layer1.iba.count)
    mean1.cov <- mean(subset$layer1.iba.coverage)
    sd1.cov <- sd(subset$layer1.iba.coverage)
    mean2.cell <- mean(subset$layer2.iba.count)
    sd2.cell <- sd(subset$layer2.iba.count)
    mean2.cov <- mean(subset$layer2.iba.coverage)
    sd2.cov <- sd(subset$layer2.iba.coverage)

    temp <- rbind(temp, data.frame(group=c(x),
                                   mean.cell1=c(mean1.cell), sd.cell1=c(sd1.cell),
                                   mean.cov1=c(mean1.cov),   sd.cov1=c(sd1.cov),
                                   mean.cell2=c(mean2.cell), sd.cell2=c(sd2.cell),
                                   mean.cov2=c(mean2.cov),   sd.cov2=c(sd2.cov)))
  }

  return(temp)
}

#gen.plottable.data <- function(x) {
#  return(data.frame(get.means(x),)
#}

plot.data <- function(rawdata) {
  ggplot(data = rawdata, aes(x = injury, y = layer1.iba.count, fill=injury)) +
    geom_dotplot(binaxis = "y", stackdir = "center")# +
    #stat_summary()
}

test <- load.file("iba1_new.csv")
print(test)
plot.data(test)
#print(get.means(c(1:10)))
#print(get.means(c("a",TRUE,23)))
print(data.summary(test))
