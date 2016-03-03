packages <- c('tseries','fBasics','ggplot2','reshape','moments','sandwich','lmtest')
for (i in 1:length(packages)) if (!require(packages[i])) install.packages(packages[i])

library(tseries)
library(fBasics)
library(ggplot2)
library(reshape)
library(moments)
library(sandwich)
library(lmtest)

setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/problemsets/')

# Helper functions
# mse <- function(reg) { 
#   mse <- mean(reg$residuals**2)
#   return(mse)
# }

r.squared <- function(reg, y) {
  1-(sum((reg$residuals)**2))/(sum((y-mean(y))**2))
}

# Read and reverse data
D <- read.csv('macro-dataset.csv', sep = '\t')
D$DATE <- as.Date(D$DATE, '%Y-%m-%d')
min.date <- as.Date('2000-06-01')
max.date <- as.Date('2014-12-01')
D <- subset(D, DATE >= min.date & DATE <= max.date)
# make sure dates make sense
# summary(D$DATE)
T <- nrow(D)

# Change to monthly growth rates * 100 (with exception of CFNAI)
# dates <- as.Date(as.character(D[,1]),'%Y-%m-%d')
# prices.column.names <- c('INDPRO','MCOILWTICO','HOUST','PSAVERT','EXUSEU')
# prices <- D[,prices.column.names]
# 
# returns <- sapply(prices, function(price) { diff(log(price))*100 })
# 
# df <- data.frame(dates = as.Date(dates[2:T]), returns)
# df <- melt(df, id.vars = 'dates')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
#ggplot(df, aes(dates,value)) + geom_line(aes(colour = variable))

# or plot on different plots
#ggplot(df, aes(dates,value)) + geom_line() + facet_grid(variable ~ .)

# take a vector and return a list of moments
my.moments <- function(returns) {
  moments.list <- list(
    mean = mean(returns),
    sd = sd(returns),
    skewness = skewness(returns),
    kurtosis = kurtosis(returns))
  lapply(moments.list, round, 3)
}


# Doing things 'his' way
T     <- length(D$DATE)
dip    <- diff(log(D$INDPRO))*100
dmcoil <- diff(log(D$MCOILWTICO))*100
dhoust <- diff(log(D$HOUST))*100
dpsavert  <- diff(log(D$PSAVERT))*100
dexuseu  <- diff(log(D$EXUSEU))*100
# Q1.
my.moments(dip)

# Q2.
# Question: Should max.lag be specified?
#   or does 'over the full sample' mean max.lag should be T?
round(acf(dip)$acf[2:6],3)
# These are the same
round(acf(dip, lag.max = T)$acf[2:6],3)

# Q3.
jb <- jarque.bera.test(dip)
round(jb$statistic,3)
round(jb$p.value,3)

# Q4.
lj.box <- Box.test(dip, lag = 6)
round(lj.box$statistic,3)
round(lj.box$p.value,3)

# Q5
data <- data.frame(y=dip[2:T],
                   dip=dip[1:(T-1)],
                   dmcoil=dmcoil[1:(T-1)],
                   dhoust=dhoust[1:(T-1)],
                   dpsavert=dpsavert[1:(T-1)],
                   dexuseu=dexuseu[1:(T-1)])


ar.reg <- lm(y ~ dip, data = data)
vc.nw <- NeweyWest(ar.reg)

round(coeftest(ar.reg),3)
round(coeftest(ar.reg,vc.nw),3)
summary(ar.reg)

# Q5-Q7

arx.reg <- lm(y ~ ., data = data)
vc.nw <- NeweyWest(arx.reg)

round(coeftest(arx.reg),3)
round(coeftest(arx.reg,vc.nw),3)
summary(arx.reg)
# Q8-Q11

arx.residuals <- residuals(arx.reg)
my.moments(arx.residuals)
# Q12.

# Q13.
# Question should this be max.lag=6?
round(acf(arx.residuals)$acf[2:6],3)

# Q14.
jb <- jarque.bera.test(arx.residuals)
round(jb$statistic,3)
round(jb$p.value,3)

# Q15.
lj.box <- Box.test(arx.residuals, lag = 6)
round(lj.box$statistic,3)
# Q15. stat: 
round(lj.box$p.value, 3)
# Q15. p-value

# Q16. ??

# Forecasting, break = 2006-12
dates <- as.Date(D$DATE,'%Y-%m-%d')
is <- dates[2:T] < as.Date('2006-12-01','%Y-%m-%d')
os <- dates[2:T] >= as.Date('2007-01-01','%Y-%m-%d') 

# In-sample data
data.is <- data[is,]
# Out-of-sample data
data.os <- data[os,]

# Actual returns, in- and out-of-sample
y.is     <- data[is,'y']
y.os     <- data[os,'y']

# In-sample, simple AR model (dip lag-1 only)
pred.ar.reg <- lm(y ~ dip, data=data.is)
# Predict out-of-sample using the simple-AR model
y.hat   <- predict(pred.ar.reg, newdata=data.os)

r.squared(pred.ar.reg, data.is[,'y'])
# mse of predictions
round(mean(na.omit(y.hat-data.os[,'y'])**2),3)

# MSE for AR model with all explanatory variabls
pred.arx.reg <- lm(y ~ ., data=data.is)
y.hat   <- predict(pred.arx.reg, newdata=data.os)

# This r-squared is wrong
r.squared(pred.arx.reg,data.is[,'y'])
round(mean(na.omit(y.hat-data.os[,'y'])**2),3)
