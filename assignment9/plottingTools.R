# loads rquired libraries
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))
library(ggsignif)

# imports a iba data file
load.file <- function(filename) {
  return(read.csv(filename))
}

# this function is used in the driver to ensure that the provided command line options are valid
check.args <- function(x) {
  valid <- c('plot1', 'plot2')
  if ((length(x) != 2) || !(x[2] %in% valid) || !file.exists(x[1])) {
    cat("Please supply a valid filename followed by either \'plot1\' or \'plot2\'.\n")
    cat("   plot1: plots a dotplot comparing mouse brain injury and IBA1+ cell coverage in the first cortical layer.\n")
    cat("   plot2: plots a density chart comparing IBA1+ cell coverage in both layers 1 and 2/3 of the brain following injury.\n")
    cat("Example: Rscript generatePlots.R plot1\n")
    quit()
  }
}

# fucntion that checks which chart is desired and then runs the appropriate functions
primary.driver <- function(x) {
  iba.data <- load.file(x[1])
  if (x[2] == 'plot1') {
    plot.data(iba.data, data.summary(iba.data))
  } else if (x[2] == 'plot2') {
     plot.data.2(iba.data)
  }
}

# required to generate the SD bars in the dotplot, esentially generates the SD and mean values
data.summary <- function(rawdata) {
  datasummary <- rawdata %>%
    group_by(injury) %>%
    summarise(
      sd = sd(layer1.iba.coverage, na.rm = TRUE),
      layer1.iba.coverage = mean(layer1.iba.coverage)
    )
  return(datasummary)
}

# plot data performs the function of plot1, that is, it plots the injury extent vs IBA+ coverage int he first layer of the cortex of the brain.
plot.data <- function(rawdata, datasummary) {
  ggplot(data = rawdata, aes(x = injury, y = layer1.iba.coverage, fill=injury)) + #fill = injury purely for aesthetic reasons, not showing any new data
    labs(x = bquote('Injury Extent'), y = bquote('IBA-1'^'+' ~'% area')) +
    scale_x_discrete(limits=c("uninjured","mild","severe")) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.2, show.legend = FALSE) +
    geom_signif( # this comes from the ggsignif library, which handles significance testing for me as well as drawing the lines and asterisks/ns
      comparisons = list(c("mild","severe"), c("uninjured", "mild"), c("uninjured", "severe")),
      test = "t.test", map_signif_level = TRUE, textsize = 4, step_increase = 0.05) +
    geom_errorbar(aes(ymin=layer1.iba.coverage-sd, ymax=layer1.iba.coverage+sd),
                  data = datasummary,
                  width=.2,
                  position=position_dodge(.2)) +
    geom_point(data=datasummary, size=2) +
    theme(legend.position = "none") +
    ggtitle(bquote('Traumatic brain injury extent vs IBA-1'^'+' ~'area coverage'), subtitle = 'Cortex, layer 1')
}

#plot data 2 draws the density graph that shows whether there is a difference between injury extent and its effect on the first and second/third layers oif the cortex.
plot.data.2 <- function(rawdata, datasummary) {
  ggplot(data=rawdata, aes(x=layer1.iba.coverage, y=layer2.iba.coverage)) +
    geom_density_2d(aes(color = injury)) +
    labs(x = bquote('Cortical layer 1'), y = bquote('Cortical layer 2/3')) +
    ggtitle(bquote('Cortical IBA1'^'+' ~'coverage vs injury extent'))
}

#test <- load.file("iba1_new.csv")
#print(test)
#plot.data(test, data.summary1(test))
#plot.data.2(test, data.summary2(test))
#plot.data.3(test, data.summary2(test))

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

#data.summary <- function(rawdata) {

#  injury.types <- unique(rawdata$injury)
#  print(injury.types)

#  temp <- data.frame(group=c(),
#                     mean.cell1=c(), sd.cell1=c(),
#                     mean.cov1=c(),  sd.cov1=c(),
#                     mean.cell2=c(), sd.cell2=c(),
#                     mean.cov2=c(),  sd.cov2=c())
#  #temp <- data.frame(group=c())

#  for (x in injury.types) {
#    subset <- rawdata[rawdata$injury == x, ]

#    mean1.cell <- mean(subset$layer1.iba.count)
#    sd1.cell <- sd(subset$layer1.iba.count)
#    mean1.cov <- mean(subset$layer1.iba.coverage)
#    sd1.cov <- sd(subset$layer1.iba.coverage)
#    mean2.cell <- mean(subset$layer2.iba.count)
#    sd2.cell <- sd(subset$layer2.iba.count)
#    mean2.cov <- mean(subset$layer2.iba.coverage)
#    sd2.cov <- sd(subset$layer2.iba.coverage)

#    temp <- rbind(temp, data.frame(group=c(x),
#                                   mean.cell1=c(mean1.cell), sd.cell1=c(sd1.cell),
#                                   mean.cov1=c(mean1.cov),   sd.cov1=c(sd1.cov),
#                                   mean.cell2=c(mean2.cell), sd.cell2=c(sd2.cell),
#                                   mean.cov2=c(mean2.cov),   sd.cov2=c(sd2.cov)))
#  }

#  return(temp)
#}

#data.summary1 <- function(rawdata) {
#  datasummary <- rawdata %>%
#    group_by(injury) %>%
#    summarise(
#      sd = sd(layer1.iba.count, na.rm = TRUE),
#      layer1.iba.count = mean(layer1.iba.count)
#    )
#  return(datasummary)
#}


#plot.data <- function(rawdata, datasummary) {
#  ggplot(data = rawdata, aes(x = injury, y = layer1.iba.count, fill=injury)) +
#    labs(x = bquote('Injury Extent'), y = bquote('IBA-1'^'+' ~' cells/mm'^2)) +
#    scale_x_discrete(limits=c("uninjured","mild","severe")) +
#    geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.2, show.legend = FALSE) +
#    geom_signif(
#      comparisons = list(c("mild","severe"), c("uninjured", "mild"), c("uninjured", "severe")),
#      test = "t.test", map_signif_level = TRUE, textsize = 4, step_increase = 0.05) +
#    geom_errorbar(aes(ymin=layer1.iba.count-sd, ymax=layer1.iba.count+sd),
#                  data = datasummary,
#                  width=.2,                    # Width of the error bars
#                  position=position_dodge(.2)) +
#    geom_point(data=datasummary, size=2) +
#    theme(legend.position = "none")
#}