if (!require('caret')) install.packages('caret')

MyTrainControl=trainControl(
  method = "repeatedCV",
  number=10,
  repeats=1,
  returnResamp = "all",
  classProbs = FALSE,
  summaryFunction=defaultSummary
)

model <- train(x = data[,2:ncol(data)],
               y = data[,'TARGET'],
               method='glmnet',
               metric = "RMSE",
               tuneGrid = expand.grid(.alpha=c(0,1),.lambda=seq(0,0.05,by=0.01)),
               trControl=MyTrainControl)
model$bestTune
model$results

model <- train(x = data[,2:ncol(data)],
               y = data[,'TARGET'],
               method='rf',
               metric = "RMSE",
               tuneGrid = expand.grid(.mtry=seq(5,50,by=5)),
               trControl = MyTrainControl)

plot(model$results$mtry, model$results$RMSE, type = 'l')
