if (!require('glmnet')) library(glmnet)
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')
source('lagged.R')
source('crossval.R')
data <- read.csv('forecast-competition-data.csv')

# data.with.everything.lagged <- lagged(1, data)
data.mat <- as.matrix(data)
x <- data.mat[,setdiff(colnames(data),'TARGET')]
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


head(cbind(alphas, mses))
# 1 lag


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

# so lambda 0 and alpha 0.01 give us the best results

if (!require('randomForest')) install.packages('randomForest')
model.function <- 'randomForest'
model.args <- list(formula = (TARGET ~ .), data, ntree = 100)
res <- cross.val.finance(model.function, model.args, data = data)
res$mse
