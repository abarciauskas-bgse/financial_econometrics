if (!require('randomForest')) install.packages(randomForest)

lagged <- function(nlag, data) {
  # target column as is minus nlag rows
  # lagged columns are target columns
  # remove contemporaneous features from data
  colnames.to.remove <- setdiff(colnames(data), 'TARGET')
  lagged.data <- data
  colnames <- colnames(data)
  for (l in 1:nlag) {
    # get the lagged values
    lagged.cols <- data[1:(nrow(data)-l),colnames]
    
    # remove top row from lagged data
    all.colnames <- colnames(lagged.data)
    lagged.data <- lagged.data[2:nrow(lagged.data),]
    lagged.data <- cbind(lagged.data, lagged.cols)
    colnames(lagged.data) <- append(all.colnames, sapply(colnames, paste0, paste0('.lag',l)))
  }
  # remove contemporaneous features from data
  lagged.data <- lagged.data[,setdiff(colnames(lagged.data),colnames.to.remove)]
  return(lagged.data)
}

predictor.lazyRF <- function(y) {
  # create the lagged data
  y.last <- y[nrow(y),]
  training.data <- lagged(1, y)
  model <- randomForest(TARGET ~ ., data = training.data)
  # make prediction only for the last period
  prediction <- predict(model, y.last)
  return(prediction)
}

# data <- read.csv('forecast-competition-data.csv')
# predictor.lazyRF(data) # should return a scalar
