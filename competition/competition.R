# Base Models
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')

data <- read.csv('forecast-competition-data.csv')
head(data)
library(corrplot)
corrplot(cor(data[2:nrow(data),]))
colnames(data)
matplot(data[,11:20], type = 'l')

predictor.team <- function(y) {
  f <- mean(y[,1])
  return(f)
}

# 0.3755085
predictor.team(data)

cross.val.finance <- function(model.function, model.args, data) {
  # Loop through the last 100 observations as the test set
  ntrain <- 400
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
    model.args$data = data
    model <- do.call(model.function, model.args)
    
    # generate one prediction using the test data
    test.data <- data[test.pointer, setdiff(colnames(data), 'TARGET')]
    pred <- predict(model, newdata = test.data)
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

## examples
model.function <- 'arima'
model.args <- list(x = data[,1], order = c(1,0,1))
res <- cross.val.finance(model.function, model.args)
print(res$mse)

model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ .', data = data)
res <- cross.val.finance(model.function, model.args)
res$mse

nlag <- 1

lagged <- function(nlag, data) {
  # target column as is minus nlag rows
  # lagged columns are target columns
  target <- data[(nlag+1):nrow(data),]
  lagged <- data[1:(nrow(data)-nlag),]
  all.with.lags <- cbind(target, lags = lagged)
  head(all.with.lags)
  return(all.with.lags)
}

data.with.everything.lagged <- lagged(1, data)

head(data.with.target.lagged[,c('TARGET','target.lagged')])
model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ target.lagged', data = data.with.target.lagged)
res <- cross.val.finance(model.function, model.args)
res$mse

# i can haz more lagz plz
model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ .')
res <- cross.val.finance(model.function, model.args, data = data.with.everything.lagged)
res$mse
