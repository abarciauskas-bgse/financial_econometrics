if (!require('glmnet')) library(glmnet)
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')
source('lagged.R')
source('crossval.R')
data <- read.csv('forecast-competition-data.csv')

data.with.everything.lagged <- lagged(3, data)
data.mat <- as.matrix(data.with.everything.lagged)
x <- data.mat[,setdiff(colnames(data.with.everything.lagged),'TARGET')]
y <- data.mat[,'TARGET']
model.function <- 'glmnet'
alphas <- seq(0,0.2,0.005)
mses <- rep(0, length(alphas))
source('crossval.R')
for (alpha.idx in 1:length(alphas)) {
  alpha <- alphas[alpha.idx]
  model.args <- list(x = x, y = y, family = 'gaussian', alpha = alpha)
  res <- cross.val.finance(model.function, model.args, data = data.with.everything.lagged, ntrain = 400)
  mses[alpha.idx] <- res$mse
}

plot(alphas, mses, type = 'l')
# 1 lag
# 0.010 0.6918977

head(cbind(alphas, mses))
# 1 lag
# 0.010 0.6918977
# 3 lags
# 0.010 0.5289332

# no we try different values of lambda
data.with.everything.lagged <- lagged(1, data)
data.mat <- as.matrix(data.with.everything.lagged)
x <- data.mat[,setdiff(colnames(data.with.everything.lagged),'TARGET')]
y <- data.mat[,'TARGET']
model.function <- 'glmnet'
alpha <- 0.01
lambdas <- seq(0,1,0.1)
mses <- rep(0, length(lambdas))

for (lambda.idx in 1:length(lambdas)) {
  lambda <- lambdas[lambda.idx]
  model.args <- list(x = x, y = y, family = 'gaussian')
  res <- cross.val.finance(model.function, model.args,
                           data = data.with.everything.lagged,
                           ntrain = 400, lambda = lambda)
  mses[lambda.idx] <- res$mse
}

plot(lambdas, mses, type = 'l')
head(cbind(lambdas, mses))
# lambdas      mses
# [1,]     0.0 0.6898902
# [2,]     0.1 0.8932667
# [3,]     0.2 1.0059228
# [4,]     0.3 1.1098562
# [5,]     0.4 1.1703382
# [6,]     0.5 1.2497988

# so lambda 0 and alpha 0.01 give us the best results
cross.val.finance()

