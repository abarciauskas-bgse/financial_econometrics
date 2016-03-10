library(caret)
setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')
source('lagged.R')
data <- read.csv('forecast-competition-data.csv')

# BASE MODEL: 1 LAG
d1 <- lagged(1, data)
base.model.args <- list(formula = TARGET ~ ., data = d1)
cv.d1 <- cross.val.finance(base.model.f, model.args = base.model.args, data = d1, ntrain = 400)
cv.d1$mse

d2 <- lagged(2, data)
base.model.args <- list(formula = TARGET ~ ., data = d2)
cv.d2 <- cross.val.finance(base.model.f, model.args = base.model.args, data = d2, ntrain = 400)
cv.d2$mse

acf(data$TARGET)
