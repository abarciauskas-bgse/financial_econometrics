setwd('~/Box Sync/abarciausksas/myfiles/12F005 Financial Econometrics/problemset/data')

data <- read.csv('series-39.csv')

in.sample <- data[1:1008,]
out.of.sample <- data[1009:nrow(data),]

plot(in.sample, type ='l')
# Assuming these don't need to be translated into returns

# LBQ Test
# Q1.
lb.test <- Box.test(in.sample, lag=22, type="Ljung-Box")
round(lb.test$statistic,4)
# X-squared 
round(lb.test$p.value, 4)
# [1] 0
# Q2. YES you can conclude that the data have serial dependence
#   (e.g. null of independence can be rejected)

y <- in.sample
def.off()
acf.1 <- acf(y, ylim=c(-0.2,1) , lwd=5 , xlim=c(0,252), col='darkorange2', lag.max = 252)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
acf.1$acf[2]
acf.1$acf[6]
acf.1$acf[11]
acf.1$acf[253]

pacf.1 <- pacf(y , ylim=c(-0.2,1), lwd=5 , xlim=c(0,252), col='darkorange2', tck=0.02, lag.max = 252)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)
pacf.1$acf[1]
pacf.1$acf[5]
pacf.1$acf[10]
pacf.1$acf[252]

# Q5. 
significant.acf.corrs <- abs(acf.1$acf) > 1.96/sqrt(length(in.sample))
min(which(significant.acf.corrs == FALSE)) - 1

# Q6.
significant.pacf.corrs <- abs(pacf.1$acf) > 1.96/sqrt(length(in.sample))
min(which(significant.pacf.corrs == FALSE))

# Q7. I think it is an ARMA(1,0) - e.g. p = 1, q = 0 because the PACF drops after the first lag
#     And the ACF demonstrates a high degree of persistence.
#     http://people.duke.edu/~rnau/arimrule.htm
arma11 = arima(y,order=c(1,0,1))
ar1    = arima(y,order=c(1,0,0))
ma1    = arima(y,order=c(0,0,1))

arma11$coef
ar1$coef
ma1$coef

var(arma11$residuals)
var(ar1$residuals)
var(ma1$residuals)

arma11$loglik
ar1$loglik
ma1$loglik

# Q8, Q9, Q10
N <- length(in.sample)
(arma11_aic <- (-2*arma11$loglik+2*4)/N)
(ar1_aic    <- (-2*ar1$loglik+2*3)/N)
(ma1_aic    <- (-2*ma1$loglik+2*3)/N)

(arma11_bic <- (-2*arma11$loglik+log(N)*4)/N)
(ar1_bic    <- (-2*ar1$loglik+log(N)*3)/N)
(ma1_bic    <- (-2*ma1$loglik+log(N)*3)/N)

# Q11
library(moments)
mean(arma11$residuals)
var(arma11$residuals)
skewness(arma11$residuals)
kurtosis(arma11$residuals)

# Q12
mean(ar1$residuals)
var(ar1$residuals)
skewness(ar1$residuals)
kurtosis(ar1$residuals)

# Q13
mean(ma1$residuals)
var(ma1$residuals)
skewness(ma1$residuals)
kurtosis(ma1$residuals)

# Q14
(arma11.res.lbq <- Box.test(arma11$residuals, lag=22, type="Ljung-Box")$p.value)
# yes, reject null of independence

# Q15
(ar1.res.lbq <- Box.test(ar1$residuals, lag=22, type="Ljung-Box")$p.value)
# yes, reject null of independence

# Q15
(ma1.res.lbq <- Box.test(ma1$residuals, lag=22, type="Ljung-Box")$p.value)
# yes, reject null of independence

# FORECAST
y.out <- out.of.sample
H <- length(y.out)
ar1_pred    <- predict(ar1, n.ahead=H)
ma1_pred    <- predict(ma1, n.ahead=H)
arma11_pred <- predict(arma11, n.ahead=H)

(arma11_mse = mean((y.out - as.numeric(arma11_pred$pred))**2))
(ar1_mse    = mean((y.out - as.numeric(ar1_pred$pred))**2))
(ma1_mse    = mean((y.out - as.numeric(ma1_pred$pred))**2))

