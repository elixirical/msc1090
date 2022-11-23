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
  #columns = colnames(rawdata)
  #print(columns)
  #means = apply(as.numeric(rawdata), 2, mean, na.rm = TRUE)
  #print(summary(rawdata)$mean)
  #means <- colMeans(rawdata)
  #print(means)
  #stddevs <- apply(rawdata, 2, sd)
  #print(stddevs)
  #return(data.frame(mean=means, stddev=stddevs))

  injury.types <- unique(rawdata$injury)
  #test.groups <- unique(rawdata$)
  print(injury.types)

  #print(rawdata[rawdata$group == 0, ])

  for (x in injury.types) {
    print(x)
    print(rawdata[rawdata$injury == x, ])

    
  }

  return(TRUE)
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
