# Name: Alvin Han
# SciNet username: tmp_ahan
# Description:
#   A bunch of functions that analyze the ttc streetcar delay csv files
#   and spits out the number of delays per incident type, the average
#   minimum delay for mechanical incidents as well as the most common
#   delayed route in the month of February.

arg <- commandArgs(trailingOnly = TRUE) # takes a argument following the script
if (length(arg) == 0) {                 # if no argument, suggests usage to user
  cat("Usage example: Rscript processTTC.R ttc-streetcar-delay-data-20XX.csv \n")
  quit()
} else if (length(arg) > 1) {           # if too many args, tells user to only use one!
  cat("Please specify only one argument! \n")
  quit()
} else cat("Processing data from file:", arg, "\n") # if one arg, begins processing the files

# reads the indicated file and converts it to a data table, with all columns as character type
# colClasses = "character" because for some reason this allowed me to avoid factor data types
# later on in the code which did not behave as I would have expected it to when using cat() on them
table <- read.csv(arg, colClasses = "character", header = TRUE)

# grabs the unique incident types within the year's data
types.of.incidents <- function(x) {
  return(unique(x$Incident))
}

# calculates the number of incidents for each type of delay -- could probably be rewritten to function
# identically to most.delays.february but maybe you want us to show that we know how to use loops?
delays.per.incident <- function(x) {
  incident.types <- types.of.incidents(x) # just grabs the unique incident types using the afore-defined function
  incident.count <- sample(c(0), length(incident.types), replace = TRUE) # creates a vector with same length as the num of incidents, with all values 0 to tally each incident
  for (n in 1:nrow(table)) { # for each row in the data table...
    index.of.incident <- match(table[n, ]$Incident,incident.types)[1] # matches the incident in that row to one in the types of incidents, and returns the position of that incidene in the vector of incidents. Because it is a vector, selects the first element
    incident.count[index.of.incident] <- incident.count[index.of.incident] + 1 # then increments the corresponding tally in incident count up by 1.
  }
  return(incident.count) # returns the vector of the number of each incident
}

# returns the average minimum delay for mechanical incidents in the data table
minimum.delay <- function(x) {
  mech.inc.times <- x[x$Incident == "Mechanical", 7] # grabs a vector of all minimum delays (column 7) that are labelled mechanical incidents
  min.delay.avg <- mean(as.numeric(mech.inc.times), na.rm = TRUE) # calculates the mean of the above vector, na.rm = TRUE tells it to ignore NA values
  return(min.delay.avg) # returns the mean
}

# returns the route with the most delays in february
most.delays.february <- function(x) {
  february.incidents = x[substr(x$Report.Date, 6, 7) == "02", ] # grabs all incidents from february by matching the month in the date
  frequency = sort(table(february.incidents$Route), decreasing = TRUE) # grabs only the route numbers, then analyzes the frequency, sorting with most frequent at the top
  worst.route = names(frequency)[1] # creates vector of the names (ie. route numbers) of the sorted frequency list, and returns the first, which had the most incidents
  return(worst.route) # returns that route number from above
}

# function that prints out the desired output as specified in the assignment
print.output <- function(x) {
  incidentTypes = types.of.incidents(x) # grabs the unique incident types so that this function doesnt have to run more then once
  numIncidents = delays.per.incident(x) # grabs the delays per incident type so that this fn wont fun more then once too
  cat("Total number of delays per incident type:\n")
  for (n in 1:length(incidentTypes)) { # for each incident type...
    cat("\t", incidentTypes[n], " -- ", numIncidents[n], "\n") # print the incident and how many times its occured
  }
  cat("The average minimum delay of the stretcars due to a mechanical incident, ignoring unreported data, is",
      minimum.delay(x), "minutes.\n")
  cat("The route with the most delays in February was route", most.delays.february(x), "\n")
}

print.output(table) # runs the above fn to actually print it
