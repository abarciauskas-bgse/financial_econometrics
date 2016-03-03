
rm( list=ls() )

library(fGarch)
library(tseries)
library(moments)

source('~/Box Sync/abarciausksas/myfiles/12F005 Financial Econometrics/code/code/lib/utilities.R')
source('~/Box Sync/abarciausksas/myfiles/12F005 Financial Econometrics/code/code/volatility-modeling/tarch.R')

save.plots = TRUE

library(moments)

D <- read.table('EFA-returns.csv')

dates <- as.Date(as.character(D[,1]),'%Y-%m-%d')
ret   <- D[,2]

# Volatility Clustering
if( save.plots ) pdf(file='EFA-abs-returns.pdf',width=7,height=5)

myplot( dates , abs(ret) , col='red2' )
abline(h=0 , lwd=2)

if( save.plots ) dev.off()

# ACFs
# ACF ret
if( save.plots) pdf(file='EFA-ret-acf.pdf',width=8,height=6)

par( mar=c(2,2,0.1,0.1) )
ret.acf <- acf(ret , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')

if( save.plots) dev.off()

# ACF |ret|
if( save.plots) pdf(file='EFA-aret-acf.pdf',width=8,height=6)

par( mar=c(2,2,0.1,0.1) )
ret.acf <- acf(abs(ret) , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
#q1
ret.acf[1]
#q1
ret.acf[5]
#q1
ret.acf[22]
if( save.plots) dev.off()

# ACF ret^2
if( save.plots) pdf(file='EFA-sqrret-acf.pdf',width=8,height=6)

par( mar=c(2,2,0.1,0.1) )
ret.acf <- acf(ret**2 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')

if( save.plots) dev.off()

# ARCHLM
y <- ret[4:length(ret)]**2
n <- length(y)
X = cbind( ret[3:(length(ret)-1)]**2 , ret[2:(length(ret)-2)]**2 , ret[1:(length(ret)-3)]**2 )

archlm = lm(y ~ X)
archlm.statistic <- n*summary( archlm )$r.squared
archlm.p.value <- 1-pchisq(archlm.statistic,3)

# ARCH(3)
# Aimee: changed from order = c(0,3)
arch11 <- garch(ret, order = c(1,1))

summary(arch11)

# vol
sigma <- arch11$fitted.values[,1]

if( save.plots ) pdf(file='EFA-arch-avol.pdf',width=7,height=5)
myplot( dates , sqrt(252)*sigma , col='red2' )
if( save.plots ) dev.off()

#
z <- ret/sigma
if( save.plots ) pdf(file='EFA-arch-stdres.pdf',width=7,height=5)
myplot( dates , z , col='red2' )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='EFA-ret-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqnorm(ret,col='tomato',main='')
qqline(ret,lwd=2,lty=3)
if( save.plots ) dev.off()

if( save.plots ) pdf(file='EFA-arch-stdres-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqnorm(z,col='tomato',main='')
qqline(z,lwd=2,lty=3)
if( save.plots ) dev.off()

jb.r <- jarque.test(ret)
jb.z <- jarque.test(z[4:length(ret)])

if( save.plots) pdf(file='EFA-absret-acf100.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
acf( abs(ret) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
if( save.plots) dev.off()

if( save.plots) pdf(file='EFA-arch-stdres-acf100.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
acf( abs(z[4:length(z)]) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
if( save.plots) dev.off()

# GARCH
garch11 <- garch(ret, order = c(1,1))

summary(garch11)

sigma <- garch11$fitted.values[,1]

N <- nrow(D)
(aic <- (-2*logLik(garch11)+2*3)/N)
(bic <- (-2*logLik(garch11)+log(N)*4)/N)

# q5 persistence
(pers <- coefficients(garch11)['a0'] + coefficients(garch11)['b1'])

# q6 annualized volatility
ann.volatility <- matrix(sqrt(252*sigma))
rownames(ann.volatility) <- as.character(dates)
head(ann.volatility)
my.dates <- c('2004-04-13', '2007-08-15', '2009-10-21', '2010-04-15', '2013-08-27')
for (i in 1:length(my.dates)) {
  print(paste0('Annualized volatility on ', my.dates[i], ': ', ann.volatility[my.dates[i],1]))
}

kurtosis(D[,2])
kurtosis(na.omit(garch11$residuals))
(res <- acf(na.omit(garch11$residuals), lag = 5))

if( save.plots ) pdf(file='EFA-garch-avol.pdf',width=7,height=5)
myplot( dates , sqrt(252)*sigma , col='red2' )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='EFA-garch-returns-and-vol.pdf',width=7,height=5)
myplot( dates , ret , col='orange2' )
lines( dates , 1.96*sigma , col='blue2')
lines( dates , -1.96*sigma , col='blue2')
if( save.plots ) dev.off()

#
z <- ret/sigma
if( save.plots ) pdf(file='EFA-garch-stdres.pdf',width=7,height=5)
myplot( dates , z , col='red2' )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='EFA-ret-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqnorm(ret,col='tomato',main='')
qqline(ret,lwd=2,lty=3)
if( save.plots ) dev.off()

if( save.plots ) pdf(file='EFA-garch-stdres-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqnorm(z,col='tomato',main='')
qqline(z,lwd=2,lty=3)
if( save.plots ) dev.off()

jb.r <- jarque.test(ret)
jb.z <- jarque.test(z[2:length(ret)])

if( save.plots) pdf(file='EFA-absret-acf100.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
acf( abs(ret) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
if( save.plots) dev.off()

if( save.plots) pdf(file='EFA-garch-stdres-acf100.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
acf( abs(z[4:length(z)]) , lag.max=100 , ylim=c(-0.1,1) , lwd=5 , xlim=c(0,25) , col='darkorange2')
if( save.plots) dev.off()


## ASYMMETRIC VOLATILITY

T     <- length(ret)

# LEV PLOT
if( save.plots) pdf(file='../images/sp500-levplot.pdf',width=8,height=6)

r.plus  <- ret[1:(T-1)]*(ret[1:(T-1)]>0) 
r.minus <- ret[1:(T-1)]*(ret[1:(T-1)]<0) 
levreg  <- lm( abs(ret[2:T]) ~ 0+r.plus + r.minus )

# GARCH
garch11 <- garch(ret, order = c(1,1))

summary(garch11)

sigma <- garch11$fitted.values[,1]

z     <- ret/sigma

z.plus  <- z[1:(T-1)]*(z[1:(T-1)]>0) 
z.minus <- z[1:(T-1)]*(z[1:(T-1)]<0) 
levreg  <- lm( abs(z[2:T]) ~ 0+z.plus + z.minus )


# TESTS
neg.dummy          <- ret[1:(T-1)]<0
neg_sign_bias.test <- lm( z[2:T]**2 ~ neg.dummy )

# The test statistics are the t-stats of the c1 coefficients in the two regressions
# q11
summary( neg_sign_bias.test )

neg.size           <- ret[1:(T-1)]*(ret[1:(T-1)]<0)
neg_size_bias.test <- lm( z[2:T]**2 ~ neg.size )

summary( neg_size_bias.test )

# TARCH & EGARCH
source('~/Box Sync/abarciausksas/myfiles/12F005 Financial Econometrics/code/code/volatility-modeling/egarch.R')
garch11  <- garch(ret, order = c(1,1))
(tarch11  <- Tgarch11(ret))
names(tarch11)
egarch11 <- Egarch(ret)

# 
vol.garch  <- garch11$fitted.values[,1]
vol.tarch  <- tarch11$volatility
vol.garch[2]
vol.garch[6]
vol.garch[23]
vol.garch[127]
vol.garch[253]

vol.tarch[1]
vol.tarch[5]
vol.tarch[22]
vol.tarch[126]
vol.tarch[252]

sov.crisis <- dates >= as.Date('2011-05-01') & dates <= as.Date('2011-10-01') 
myplot( dates[sov.crisis] , cumsum( ret[sov.crisis] ) )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-vol-cmp-zoom.pdf',width=7,height=5)
myplot( dates[sov.crisis] , sqrt(252)*vol.garch[sov.crisis] , col='red2' , ylim=c(0,60) )
lines( dates[sov.crisis] , sqrt(252)*vol.tarch[sov.crisis] , col='blue2' )
lines( dates[sov.crisis] , sqrt(252)*vol.egarch[sov.crisis] , col='green2' )
legend( 'topleft' , c('garch','tarch','egarch') , col=c('red2','blue2','green2') , lwd=3)
if( save.plots ) dev.off()

# residual diagnostics
z.tarch <- ret/vol.tarch
z.egarch <- ret/vol.egarch

neg.dummy                 <- ret[1:(T-1)]<0
neg_sign_bias.test        <- lm( z[2:T]**2 ~ neg.dummy )
neg_sign_bias.tarch.test  <- lm( z.tarch[2:T]**2 ~ neg.dummy )
neg_sign_bias.egarch.test <- lm( z.egarch[2:T]**2 ~ neg.dummy )

x.range <- seq(-5,5,0.01)
nicg <- sqrt(252)*sqrt((coef(garch11)['a0'] + coef(garch11)['b1'] * var(ret) ) + coef(garch11)['a1'] * x.range**2)
nict <- sqrt(252)*sqrt((tarch11$par['omega'] + tarch11$par['beta'] * var(ret) ) + (tarch11$par['alpha'] + tarch11$par['gam1']*(x.range<0)) * x.range**2)
# THIS IS EMBARASSING!
nice <- sqrt(252)*sqrt( exp( -0.09962587 + 0.9805331 * log(var(ret)) + (0.09962587 * (abs(x.range)/sd(ret) -sqrt(2/pi))  - 0.1282174*x.range/sd(ret))  ) )

if( save.plots ) pdf(file='../images/sp500-nic.pdf',width=7,height=5)
plot (x.range,nicg,lwd='2',col='red2',t='l',ylim=c(15,40),tck=0.02,ylab='NIC',xlab='')
lines(x.range,nict,lwd='2',col='blue2',t='l')
lines(x.range,nice,lwd='2',col='green2',t='l')
grid()
if( save.plots ) dev.off()

