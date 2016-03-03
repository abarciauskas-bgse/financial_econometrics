cross.val.finance <- function(model.function, model.args, data, ntrain = 400) {
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
    train.data <- data[train.pointer:(ntrain+train.pointer-1),]
    # train model
    model <- do.call(model.function, model.args)
    
    # generate one prediction using the test data
    # if using glmnnet do something else
    test.data <- as.matrix(data[test.pointer, setdiff(colnames(data), 'TARGET')])
    pred <- NA
    if (all(class(mod) == c('elnet','glmnet'))) {
      pred <- predict(model, newx = as.matrix(test.data), s = 0.01)
    } else {
      pred <- predict(model, newdata = test.data)
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
