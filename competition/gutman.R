rm(list = ls())

setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/competition/')

data <- read.csv('forecast-competition-data.csv')




lagged <- function(data,lag){
  # Cut input data by lag specification
  lagged <- data[1:(nrow(data)-lag),]
  # Return lagged data frame
  return(lagged)
}

cutted <- function(data,cut){
  # Cut the beginning of input data frame by cut specification
  cutted <- data[(1+cut):length(data)]
  # Return cutted data frame
  return(cutted)
}

if (!require("glmnet")) install.packages("glmnet"); library(glmnet)
if (!require("tseries")) install.packages("tseries"); library(tseries)
if (!require("forecast")) install.packages("forecast"); library(forecast)

################################################################################
# Summary
################################################################################
plot(data$TARGET,type='l')
dim(data)

acf(data$TARGET)
################################################################################
# Some benchmark moddels
################################################################################

y <- data$TARGET
x <- data[,!(names(data) %in% c('TARGET'))]

xlag <- as.matrix( lagged( x , 1 ) )
ycut <- as.matrix( cutted( y , 1 ) )

split  <- nrow(data) * 0.9
nsplit <- nrow(data) -split

xtr <- xlag[1:split,]
ytr <- ycut[1:split]

xte <- xlag[(split+1):nrow(xlag), ]
yte <- ycut[(split+1):nrow(ycut)  ]

# train
m01 <- glmnet( xtr , ytr , alpha = 1 )
m02 <- glmnet( xtr , ytr , alpha = 0.5 )
m03 <- glmnet( xtr , ytr , alpha = 0 )

# predict
LASpred <- predict( m01, newx=xte, s=0.01)
ELApred <- predict( m02, xte,s=0.01)
RIDpred <- predict( m03 , xte,s=0.04)

tLAS <-ts(LASpred,start=1,end=50,frequency = 1)
tRID <-ts(RIDpred,start=1,end=50,frequency = 1)
tELA <-ts(ELApred,start=1,end=50,frequency = 1)

# Compute accuracy summary
LASac <- accuracy(tLAS,yte)
RIDac <- accuracy(tRID,yte)
ELAac <- accuracy(tELA,yte)

plot( 1:length(yte), yte, type = 'l', xlim=c(0,nsplit),ylim = c( -4, 5 )  )
par( new = TRUE )
plot( 1:length(yte), LASpred, type = 'l',xlim=c(0,nsplit), ylim = c( -4, 5 ),col = 'blue'  )
par( new = TRUE )
plot( 1:length(yte), RIDpred, type = 'l',xlim=c(0,nsplit), ylim = c( -4, 5 ),col = 'red'  )
par( new = TRUE )
plot( 1:length(yte), ELApred, type = 'l',xlim=c(0,nsplit), ylim = c( -4, 5 ),col = 'green'  )

mean((LASpred - yte)**2)
mean((ELApred - yte)**2)
mean((RIDpred - yte)**2)
nsplit
