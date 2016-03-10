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
    model <- NA
    train.rowset <- train.pointer:(ntrain+train.pointer-1)
    train.data <- data[train.rowset,]
    model.args['data'] <- list(train.data)
    # train model
    model <- do.call(model.function, model.args)
    
    # generate one prediction using the test data
    # if using glmnnet do something else
    test.data <- data[test.pointer, setdiff(colnames(data), 'TARGET')]
    pred <- predict(model, newdata = test.data)

    preds[test.idx] <- pred
    train.pointer <- train.pointer + 1
    # for testing start at the training offset
    test.pointer <- test.pointer + 1
  }
  
  return(list(mse = mean((preds-actual)**2), preds = preds, actual = actual))
}
