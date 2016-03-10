library(caret)
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')
source('lagged.R')
data <- read.csv('forecast-competition-data.csv')

d1 <- lagged(1, data)
rf.model <- 'randomForest'
rf.args <- list(formula = TARGET ~ ., data = d1, mtry = 4)
cv.d1 <- cross.val.finance(rf.model, model.args = rf.args, data = d1, ntrain = 400)
cv.d1$mse

acf(data$TARGET)
