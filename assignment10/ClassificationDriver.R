source("ClassificationUtilities.R")

args = commandArgs(trailingOnly = TRUE)

#one driver function that runs functions from the classification utilities
driver <- function(x) {
    valid.model(x) # checks that the user supplies enough arguments, and that the second is one of SVM, DT, or KNN
    rawdata <- load.file(x[1]) # loads data from the file, and splits it into two groups as a list
    ML.model <- generate.model(as.data.frame(rawdata[1]), x[2]) # generates the appropriate model using 70% of the data
    confusion.matrix(ML.model, as.data.frame(rawdata[2])) # generates a confusion matrix and accuracy value, and prints them out to the console
}

# runs the above function
driver(args)