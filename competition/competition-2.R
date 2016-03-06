# Base Models
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')

data <- read.csv('forecast-competition-data.csv')

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
