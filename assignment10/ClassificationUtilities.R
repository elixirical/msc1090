library(caret)

# loads a file from a directory or URL; returns a list of data.frames: [training data, test data]
load.file <- function(filename) {
  # if (file.exists(filename)) {
  rawdata <- read.csv(filename)
  #}
  #else {
  #  cat("File", filename, "does not exist!\n")
  #  quit()
  #}

  split <- sample(c(T,F), nrow(rawdata), replace = T, prob = c(0.7, 0.3))

  return(list(rawdata[split,], rawdata[!split,]))
}

cv <- function() {
  return(trainControl(method = "cv", number = 10, classProbs = TRUE))
}

dt.model <- function(cv.model, traindata) {
  return(train(label ~ ., data = traindata, method = "rpart", trControl = cv.model))
}

svm.model <- function(cv.model, traindata) {
  return(train(label ~ ., data = traindata, method = "svmLinear", trControl = cv.model, preProc=c("center","scale")))
}

data.model <- function(traindata, model) {

  fitControl <- cv()

  if (model == "DT") {
    return(dt.model(fitControl, traindata))
  } else if (model == "SVM") {
    return(svm.model(fitControl, traindata))
  }
}

kNN.model <- function(traindata) {
  knnFit <- train(label ~ ., data = traindata, method = 'knn', preProcess = c('center', 'scale'), tuneLength = 10)
  return(knnFit)
}

confusion.matrix <- function(model, test.data) {
  pred <- predict(model, newdata = test.data)
  test.data$label <- factor(test.data$label)
  cm <- confusionMatrix(pred, test.data$label)
  print(cm$table)
  #return(table(test.data$label, pred))
}

test <- load.file("https://pages.scinet.utoronto.ca/~afedosee/seeds_dataset2.txt")
#print(test)
#print(test[1])
testknn <- kNN.model(as.data.frame(test[1]))
testdt <- data.model(as.data.frame(test[1]), "DT")
print(predict(testdt, newdata = as.data.frame(test[2])))
print(as.data.frame(test[2])$label)
#print(data.model(as.data.frame(test[1]), "SVM"))
#confusion.matrix(testknn, as.data.frame(test[2]))
