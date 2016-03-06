lagged <- function(nlag, data) {
  # target column as is minus nlag rows
  # lagged columns are target columns
  target <- data[(nlag+1):nrow(data),]
  lagged <- data[1:(nrow(data)-nlag),]
  all.with.lags <- cbind(target, lags = lagged)
  head(all.with.lags)
  return(all.with.lags)
}
