arg <- commandArgs(trailingOnly = TRUE)
if (length(arg) == 0) {
  cat("Usage example: Rscript processTTC.R ttc-streetcar-delay-data-20XX.csv \n")
  quit()
} else if (length(arg) > 1) {
  cat("Please specify only one argument! \n")
  quit()
}

table <- read.csv(arg, header = TRUE)

delays.per.incident <- function(x) {
  incident.types <- unique(x$Incident)
  incident.vector <- c()
  for (incident in incident.types) {
    incident.vector <- append(incident.vector, 0)
  }
  incident.count <- data.frame(Type=incident.types, Count=incident.vector)
  for (i in 1:nrow(table)) {
    if table[i, ]$Incident
  }
}

minimum.delay <- function() {

}

most.delays.february <- function() {

}

delays.per.incident(table)
