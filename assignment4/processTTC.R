arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) == 0) {
  cat("Usage example: Rscript processTTC.R ttc-streetcar-delay-data-20XX.csv \n")
  quit()
} else if (length(arg) > 1) {
  cat("Please specify only one argument! \n")
  quit()
} else cat("Processing data from file:", arg, "\n")

table <- read.csv(arg, header = TRUE)

types.of.incidents <- function(x) {
  return(unique(x$Incident))
}

print(types.of.incidents(table))

delays.per.incident <- function(x) {
  incident.types <- types.of.incidents(x)
  incident.count <- sample(c(0), length(incident.types), replace = TRUE)
  for (n in 1:nrow(table)) {
    index.of.incident <- match(table[n, ]$Incident,incident.types)[1]
    incident.count[index.of.incident] <- incident.count[index.of.incident] + 1
  }
  return(incident.count)
}

minimum.delay <- function() {
  return(0)
}

most.delays.february <- function() {
  return(0)
}

print.output <- function(incidentTypes, numIncidents, mechIncMinDelay, mostDelayedRoute) {
  cat("Total number of delays per incident type:\n")
  print(typeof(incidentTypes))
  cat(incidentTypes)
  cat(incidentTypes[1])
  for (n in 1:length(incidentTypes)) {
    cat("\t", incidentTypes[n], " -- ", numIncidents[n], "\n")
  }
  cat("The average minimum delay of the stretcars due to a mechanical incident, ignoring unreported data, is",
      mechIncMinDelay, "minutes.\n")
  cat("The route with the most delays in February was route", mostDelayedRoute, "\n")
}

print.output(types.of.incidents(table),delays.per.incident(table),minimum.delay(),most.delays.february())
