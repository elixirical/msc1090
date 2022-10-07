arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) == 0) {
  cat("Usage example: Rscript processTTC.R ttc-streetcar-delay-data-20XX.csv \n")
  quit()
} else if (length(arg) > 1) {
  cat("Please specify only one argument! \n")
  quit()
} else cat("Processing data from file:", arg, "\n")

table <- read.csv(arg, colClasses = "character", header = TRUE)
table.not.chars <- read.csv(arg)

types.of.incidents <- function(x) {
  return(unique(x$Incident))
}

delays.per.incident <- function(x) {
  incident.types <- types.of.incidents(x)
  incident.count <- sample(c(0), length(incident.types), replace = TRUE)
  for (n in 1:nrow(table)) {
    index.of.incident <- match(table[n, ]$Incident,incident.types)[1]
    incident.count[index.of.incident] <- incident.count[index.of.incident] + 1
  }
  return(incident.count)
}

minimum.delay <- function(x) {
  y <- x[x$Incident == "Mechanical", 7]
  min.delay.avg <- mean(as.numeric(y), na.rm = TRUE)
  return(min.delay.avg)
}

unique.routes <- function(x) {
  return(unique(x$Route))
}

most.delays.february <- function(x) {
  february.incidents = x[substr(x$Report.Date, 6, 7) == "02", ]
  frequency = sort(table(february.incidents$Route), decreasing = TRUE)
  worst.route = names(frequency)[1]
  return(worst.route)
}

print.output <- function(incidentTypes, numIncidents, mechIncMinDelay, mostDelayedRoute) {
  cat("Total number of delays per incident type:\n")
  for (n in 1:length(incidentTypes)) {
    cat("\t", incidentTypes[n], " -- ", numIncidents[n], "\n")
  }
  cat("The average minimum delay of the stretcars due to a mechanical incident, ignoring unreported data, is",
      mechIncMinDelay, "minutes.\n")
  cat("The route with the most delays in February was route", mostDelayedRoute, "\n")
}

print.output(types.of.incidents(table),delays.per.incident(table),minimum.delay(table),most.delays.february(table))
