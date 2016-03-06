lagged <- function(nlag, data) {
  # target column as is minus nlag rows
  # lagged columns are target columns
  lagged.data <- data
  colnames <- colnames(data)
  for (l in 1:nlag) {
    # get the lagged values
    lagged.cols <- data[1:(nrow(data)-l),colnames]
    
    # remove top row from lagged data
    all.colnames <- colnames(lagged.data)
    lagged.data <- lagged.data[2:nrow(lagged.data),]
    lagged.data <- cbind(lagged.data, lagged.cols)
    colnames(lagged.data) <- append(all.colnames, sapply(colnames, paste0, paste0('.lag',l)))
  }
  return(lagged.data)
}
