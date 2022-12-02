library(caret)

# prints out an error message and quits the program; called if the file doesnt load properly, or if the supplied arguments are incorrect 
argument.failure <- function() {
  cat("ERROR: Possible invalid use of ClassificationUtilities.R. Please select indicate a valid filepath followed by any of the following 3 models: KNN, SVM, DT.\n")
  quit()
}

# checks that the user asks for a valid model (SVN, DT, or KNN)
valid.model <- function(x) {
  valid = c("SVM", "DT", "KNN")
  if ((!x[2] %in% valid) || (length(x) != 2)) {
    argument.failure()
  }
}

# selects the appropriate function to run for the model the user desires, then runs and returns it
generate.model <- function(traindata, model) {
  if (model %in% c("SVM", "DT")) {
    return(data.model(traindata, model))
  } else return(kNN.model(traindata))
} 

# loads a file from a directory or URL; returns a list of data.frames: [training data, test data]
# the try catch stuff was me trying to be fancy but i dont think its actually,,, any use but I left it in cause i think its cool
load.file <- function(filename) {
  tryCatch(
    expr = {
      cat("Loading data from", filename, "...\n")
      rawdata <- read.csv(filename)
      rawdata$label <- sapply(rawdata$label, toString)
      cat("Splitting data 70/30 into training and test groups...\n")
      split <- sample(c(T,F), nrow(rawdata), replace = T, prob = c(0.7, 0.3))
      return(list(rawdata[split,], rawdata[!split,]))
    },
    error = function(e){
      message('Error loading file:')
      print(e)
      argument.failure()
    }, 
    warning = function(w){
      message('Possible issues loading file:')
      print(w)
      argument.failure()
    }
  )
}

# this function generates and returns either a SVN or DT model. 
data.model <- function(traindata, model) {
  fitControl <- trainControl(method = "cv", number = 10, classProbs = FALSE)

  if (model == "DT") {
    cat("Generating decision tree model using 10-fold cross validation...\n")
    dt.model <- train(make.names(label) ~ ., data = traindata, method = "rpart", trControl = fitControl)
    return(dt.model)
  } else if (model == "SVM") {
    cat("Generating support vector tree algorithm using 10-fold cross validation...\n")
    svm.model <- train(make.names(label) ~ ., data = traindata, method = "svmLinear", trControl = fitControl, preProc=c("center","scale"))
    return(svm.model)
  }
}

# generates and returns a nearest neighbour model with k=10
kNN.model <- function(traindata) {
  cat("Generating nearest neighbour model using k = 10...\n")
  knnFit <- train(make.names(label) ~ ., data = traindata, method = 'knn', preProcess = c('center', 'scale'), tuneLength = 10)
  return(knnFit)
}

# generates the confusion matrix and prints out  the matrix and accuracy 
confusion.matrix <- function(model, test.data) {
  cat("Predicting categorization of the test group...\n")
  pred <- predict(model, newdata = test.data)
  test.data$label <- factor(make.names(test.data$label))
  cat("Generating confusion matrix...\n")
  cm <- confusionMatrix(pred, test.data$label)
  cat("Confusion matrix:\n")
  print(cm$table)
  cat("Accuracy:", cm$overall["Accuracy"], "\n")
}

#test <- load.file("https://pages.scinet.utoronto.ca/~afedosee/seeds_dataset2.txt")
#test <- load.file("asdasd.txt")
#test <- load.file("https://pages.scinet.utoronto.ca/~afedosee/cars_labeled.csv")
#print(test)
#print(as.data.frame(test[2]))
#testknn <- kNN.model(as.data.frame(test[1]))
#print(testknn)
#testdt <- data.model(as.data.frame(test[1]), "SVM")
#print(predict(testdt, newdata = as.data.frame(test[2])))
#print(as.data.frame(test[2])$label)
#print(data.model(as.data.frame(test[1]), "SVM"))
#confusion.matrix(testdt, as.data.frame(test[2]))
