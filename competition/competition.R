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

## examples
model.function <- 'arima'
model.args <- list(x = data[,1], order = c(1,0,0))
res <- cross.val.finance(model.function, model.args, data = data)
print(res$mse)

model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ .', data = data)
res <- cross.val.finance(model.function, model.args, data)
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

# i can haz more lagz plz
model.function <- 'lm'
model.args <- list(formula = 'TARGET ~ .')
res <- cross.val.finance(model.function, model.args, data = data.with.everything.lagged)
res$mse
plot(res$actual, type = 'l', ylim = c(-4,5))
lines(res$preds, col = 'green')
# > res$mse
# [1] 0.6869138

# sanity check
m <- lm(TARGET ~ ., data = data.with.everything.lagged[1:400,])
preds <- predict(m, newdata = data.with.everything.lagged[401:499,])
mean((preds-data.with.everything.lagged[401:499,'TARGET'])**2)
# [1] 1.066627

lagcols <- colnames(data.with.everything.lagged)[which(sapply(colnames(data.with.everything.lagged), function(colname) {
  grepl("lags", colname)
}))]

model.function <- 'lm'
model.args <- list(formula = paste('TARGET ~', paste(lagcols, collapse = '+')))
res <- cross.val.finance(model.function, model.args, data = data.with.everything.lagged)
res$mse
# > res$mse
# [1] 1.072224

library(glmnet)
data.mat <- as.matrix(data)
x <- data.mat[,setdiff(colnames(data),'TARGET')]
y <- data.mat[,'TARGET']
source('crossval.R')
model.function <- 'glmnet'
model.args <- list(x = x, y = y, family = 'gaussian', alpha = 0.00)

res <- cross.val.finance(model.function, model.args, data = data, ntrain = 400)
res$mse
