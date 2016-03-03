if (!require('tseries')) install.packages('tseries')
library(tseries)
library(fBasics)

setwd('~/Box\ Sync/abarciausksas/myfiles/Financial\ Econometrics/problemsets')
data.og <- read.csv('EFA.csv')
T <- nrow(data.og)

data.og <- data.og[seq(T,1,-1),]
plot(data.og[,'Adj.Close'], type = 'l', col = 'red2')

# construct returns dates
dates <- as.Date(as.character( data.og[,'Date'] ) , '%Y-%m-%d')

# construct prices
price <- data.og[,'Adj.Close']

returns <- diff(log( price ))*100
plot(dates[2:T], returns, col='red2', type = 'l')
abline(h=0 , lwd=2)

# pack dates and returns in a data.og frame and write it to disk
data.returns <- cbind.data.frame(dates[2:T], returns)
colnames(data.returns) <- c('date','returns')
write.table(data.returns,'EFA-returns.csv', sep=' ', row.names=FALSE, col.names=FALSE)

# Carry out an ADF test to assess the evidence of nonstationarity
#
(res <- adf.test(data.returns[,'returns']))
# Dickey-Fuller 
# Q1. -14.72578
# Q2. Yes, we reject the null of nonstationarity

(returns.mean <- mean(returns))
# Q3. 0.019
(returns.var <- var(returns))
# Q4. 2.098
(returns.skewness <- mean(((returns - returns.mean)^3)/(sd(returns)^3)))
# same as skewness(returns)
# Q5. -0.05799767
# Q6. Negatively skewed
(returns.kurtosis <- mean(((returns - returns.mean)^4)/(sd(returns)^4)))
# Q7. 13.622
# library(kurtosis)
# (nearly) the same as kurtosis(returns) => 13.629
# Q8. fat-tailed

(max(returns))
# Q9. 14.74508
(min(returns))
# Q9. -11.83689

(sum(abs(returns)>5)/length(returns))*100
# Q10. 1.220

jb <- jarque.bera.test(returns)
jb$statistic
# Q11. 16986.69
jb$p.value
# Q12. 0
# Q13. yes

# Annualized Volatility
# Ques: Am I doing this right?
(sd(returns)*sqrt(252))
# Q14. 22.99501
data.2007 <- subset(data.returns, date > as.Date('2006-12-31') & date < as.Date('2008-01-01'))
(sd(data.2007[,'returns'])*sqrt(252))
# Q15. 18.372

data.2009 <- subset(data.returns, date > as.Date('2008-12-31') & date < as.Date('2010-01-01'))
(sd(data.2009[,'returns'])*sqrt(252))
# Q16. 31.725

data.2014 <- subset(data.returns, date > as.Date('2013-12-31') & date < as.Date('2015-01-01'))
(sd(data.2014[,'returns'])*sqrt(252))
# Q17. 12.394

acf <- acf(returns, lag.max = 126)
acf$acf[2:6] # removing first lag as the 0th lag
# Q18. -0.101393185 -0.029881711  0.014998492 -0.007695878 -0.034587553

zero.confidence.threshold <- 1.96 * 1/sqrt(length(returns))
sum(abs(acf$acf)>zero.confidence.threshold)-1
# Q19. 25

plot(acf)
abline(h=0.05, col='red')
abline(h=-0.05, col='red')

# Subtracting 1 as the 0th lag
sum(abs(acf$acf)>0.05)-1
# Q20. 9 

# Q19(2). Short the asset and long the market because change in returns is negative
