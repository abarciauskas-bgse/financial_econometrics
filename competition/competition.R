# Base Models
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')

data <- read.csv('forecast-competition-data.csv')
head(data)
library(corrplot)
corrplot(cor(data))
colnames(data)
matplot(data[,11:20], type = 'l')

predictor.team <- function(y) {
  f <- mean(y[,1])
  return(f)
}

# 0.3755085
predictor.team(data)

cross.val.finance <- function(model.function, model.args) {
  # Loop through the last 100 observations as the test set
  ntest <- 100
  ntrain <- 400
  # for training start at the beginning
  train.pointer <- 1
  # for testing start at the training offset
  test.pointer <- ntrain + 1
  # init a vector of predictions
  preds <- rep(NA, ntest)
  actual <- data[test.pointer:(ntrain+ntest),'TARGET']
  
  for (test.idx in 1:ntest) {
    train.data <- data[train.pointer:(ntrain+train.pointer),]
    # train model
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

nlags <- 1
data$target.lagged <- c(rep(NA,nlags),data[nlags:(nrow(data)-nlags),'TARGET'])
head(data[,c('TARGET','target.lagged')])
model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ target.lagged', data = data)
res <- cross.val.finance(model.function, model.args)
res$mse
