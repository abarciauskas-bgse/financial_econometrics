cross.val.finance <- function(
  model.function,
  model.args,
  data,
  ntrain = 400,
  lambda = 0,
  ntree = 100) {
  # Loop through the last 100 observations as the test set
  ntrain <- ntrain
  ntest <- nrow(data) - ntrain
  # for training start at the beginning
  train.pointer <- 1
  # for testing start at the training offset
  test.pointer <- ntrain + 1
  # init a vector of predictions
  preds <- rep(NA, ntest)
  actual <- data[test.pointer:(ntrain+ntest),'TARGET']
  
  for (test.idx in 1:ntest) {
    model <- NA
    train.rowset <- train.pointer:(ntrain+train.pointer-1)
    if (model.function == 'glmnet') {
      x.train <- as.matrix(data[train.rowset,setdiff(colnames(data),'TARGET')])
      y.train <- data[train.rowset,'TARGET']
      model.args['x'] <- list(x.train)
      model.args['y'] <- NA
      model.args['y'] <- list(y.train)
      model <- do.call(model.function, model.args)
    } else {
      train.data <- data[train.rowset,]
      model.args['data'] <- list(train.data)
      # train model
      model <- do.call(model.function, model.args)
    }
    
    # generate one prediction using the test data
    # if using glmnnet do something else
    test.data <- as.matrix(data[test.pointer, setdiff(colnames(data), 'TARGET')])
    pred <- NA
    if (model.function == 'glmnet') {
      pred <- predict(model, newx = as.matrix(test.data), s = lambda)
    } else {
      pred <- predict(mod1, newdata = as.data.frame(test.data))
    }
    
    if (mode(pred) == 'numeric') {
      preds[test.idx] <- pred
    } else if (mode(pred) == 'list') {
      preds[test.idx] <- pred$pred
    }
    train.pointer <- train.pointer + 1
    # for testing start at the training offset
    test.pointer <- test.pointer + 1
  }
  
  return(list(mse = mean((preds-actual)**2), preds = preds, actual = actual))
}
