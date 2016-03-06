rm( list=ls() )

setwd('~/Box Sync/abarciausksas/myfiles/Financial Econometrics/problemsets/')

library(fGarch)
library(tseries)
library(moments)

source('ps5-utilities.R')
source('tarch.R')

# For convenience
save.plots = FALSE

D <- read.table('EFA-returns.csv');

dates <- as.Date(as.character(D[,1]),'%Y-%m-%d')
ret   <- D[,2]
ret[ ret < -200 ] <- 0
    
# TARCH
# Tarch11 n is assuming the distribution of random draws as normal
# st is drawn from student-t

tarch11.n   <- Tgarch11(ret)
tarch11.st  <- Tgarch11(ret, cond.dist='std')

# The parameters of the 2 models,  first normal then student-t 

print( round( cbind( c(tarch11.n$par,0) , tarch11.st$par ) , 3 ) )

# Volatility measures, to be used for standardizing  
sigma.n  <- tarch11.n$volatility
sigma.st <- tarch11.st$volatility

# Standardized returns
(z.n   <- ret/sigma.n)
(z.st  <- ret/sigma.st)


# QQplot is one of the analysis methods to determine if the standardized 
# returns are following the assumed distribution or not. 
# The stastics based KS test is done after this 

if( save.plots ) pdf(file='../images/sp500-tarch-n-stdres-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqnorm(z.n,col='tomato',main='')
qqline(z.n,lwd=2,lty=3)
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-tarch-st-stdres-qqplot.pdf',width=8,height=6)
par( mar=c(2,2,0.1,0.1) )
qqplot(rstd(10000,nu=tarch11.st$par['shape']),z.st,col='tomato',main='')
qqline(z.st,lwd=2,lty=3)
if( save.plots ) dev.off()

# Doing the KS test for the normal and the standardized residuals
# z.n residuals are the standardized residuals from the TARCH-N model fit
# z.st are the standardized residuals from the TARCH-T model fit

# The null hypothesis of the test is that they come from the same distribution 
# if the p-value is significant, we reject the null and determine that the 
# standardized residuals are not distributed as normal or student-t based on the 
# analysis. 
gof.n  <- ks.test( z.n , rnorm(10000,0,1) )
gof.st <- ks.test( z.st , rstd(10000,nu=tarch11.st$par['shape']) )


# residual analysis
# Calculating the pdf of the standardized residuals based ont he assumption made about the
# underlying distribution. This is to plot them and check the deviation from 1 in the 
# histogram plot. 
# No deviation implies that they are following the assumed distribution(normal/student-t)
# The U-Transform analysis plots

u.n  = pnorm( z.n );
u.st = pstd( z.st , nu=tarch11.st$par['shape'] )

if( save.plots ) pdf(file='../images/sp500-tarch-n-stdres-uhist.pdf',width=8,height=6)
par( tck=0.02 , xaxs='i' , yaxs='i' )
h.n <- hist( u.n , col='red' , freq=FALSE , ylim=c(0,1.25) )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-tarch-st-stdres-uhist.pdf',width=8,height=6)
par( tck=0.02 , xaxs='i' , yaxs='i' )
h.st <- hist( u.st , col='red' , freq=FALSE , ylim=c(0,1.25))
if( save.plots ) dev.off()

# 
# kernel
# Kernel Density Plot
kernel <- density(z.n)
kernel

if( save.plots) pdf(file='../images/sp500-tarch-n-stdres-kdens.pdf',width=8,height=6)

par( mar=c(2,2,0.1,0.1) )
plot( kernel , main='' , yaxs='i' )
polygon( kernel , col="tomato" , border='darkred' )
lines( seq(-10,10,0.1) , dnorm( seq(-10,10,0.1) , mean(ret) , sd(ret) ) , col='darkblue' ,lwd=4)

if( save.plots ) dev.off()

# Out Of Sample VaR Forecasting
# Here we forecast the Var using historical returns, TARCH-N, TARCH-ST and NP 

date.cutoff <- '2011-12-31'
is <- dates <= as.Date(date.cutoff)
os <- dates > as.Date(date.cutoff)

tarch11.n   <- Tgarch11(ret[is])
tarch11.st  <- Tgarch11(ret[is],cond.dist='std')

print( round( cbind( c(tarch11.n$par,0) , tarch11.st$par ) , 3 ) )

sigma.n  <- tarch11.n$volatility
sigma.st <- tarch11.st$volatility

theta.n  <- tarch11.n$par
theta.st <- tarch11.st$par

sig2.n   <- sigma.n[sum(is)]**2
sig2.st  <- sigma.st[sum(is)]**2

z.n      <- ret[is]/sigma.n

VaR.hs <- rep( 0 , sum( os ) )
VaR.n  <- rep( 0 , sum( os ) )
VaR.st <- rep( 0 , sum( os ) )
VaR.np <- rep( 0 , sum( os ) ) 

for( t in (sum(is)+1):length(ret) ){
    
  sig2.n <-theta.n['omega'] + theta.n['alpha']*ret[t-1]**2 + theta.n['gam1']*ret[t-1]**2 * (ret[t-1]<0) + theta.n['beta']*sig2.n
  sig2.st <-theta.st['omega'] + theta.st['alpha']*ret[t-1]**2 + theta.st['gam1']*ret[t-1]**2 * (ret[t-1]<0) + theta.st['beta']*sig2.st
  
  #
  VaR.hs[ t - sum(is) ] <- -quantile( ret[(t-501):t-1] , 0.01 )
  VaR.n[ t - sum(is) ]  <- -sqrt( sig2.n ) * qnorm(0.01)
  VaR.st[ t - sum(is) ] <- -sqrt( sig2.n ) * qstd(0.01,nu=theta.st['shape'])
  VaR.np[ t - sum(is) ] <- -sqrt( sig2.n ) * quantile( z.n , 0.01 )
}

mean(VaR.n)
mean(VaR.st)
mean(VaR.np)

dates.os <- dates[os]
ret.os   <- ret[os]
 
#Defining the hit variables for the UC and DQ tests

h.hs <- ret.os < -VaR.hs
h.n  <- ret.os < -VaR.n
h.st <- ret.os < -VaR.st
h.np <- ret.os < -VaR.np

mean(h.n)
mean(h.st)
mean(h.np)
mean(h.hs)

if( save.plots ) pdf(file='../images/sp500-condist-var-his.pdf',width=8,height=6)
myplot( dates.os , -VaR.hs , ylim=c(-15,0) )
points( dates.os[h.hs] , ret.os[h.hs] , pch=4 , lwd=2 )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-condist-var-tarch-n.pdf',width=8,height=6)
myplot( dates.os , -VaR.n , ylim=c(-15,0) , col='red2')
points( dates.os[h.n] , ret.os[h.n] , pch=4 , lwd=2 )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-condist-var-tarch-st.pdf',width=8,height=6)
myplot( dates.os , -VaR.st , ylim=c(-15,0) , col='green2' )
points( dates.os[h.st] , ret.os[h.st] , pch=4 , lwd=2 )
if( save.plots ) dev.off()

if( save.plots ) pdf(file='../images/sp500-condist-var-tarch-np.pdf',width=8,height=6)
myplot( dates.os , -VaR.np , ylim=c(-15,0) , col='purple' )
points( dates.os[h.np] , ret.os[h.np] , pch=4 , lwd=2 )
if( save.plots ) dev.off()

avg.hit <- c( mean(h.hs) , mean(h.n) , mean(h.np), mean(h.st) )
print( round(avg.hit,3) )

# The following is the uc test 
# The test works in the following way: we evaluate the number of 1's and 0's we should see
# with the number that is actually present in the data. The null is that the no. of hits 
# i.e. VaR violations are 0.01 of the whole data. If they are different, it means that 
# our model is not setting an accurate VaR value for the data. 
# The result should be close to 0.01

p <- 0.01
p <- 0.01
  uc.stat <- function(p,h){   
  uc <- -2 *(  log(p**sum(h) * (1-p)**(length(h)-sum(h)) ) - log(mean(h)**sum(h) * (1-mean(h))**(length(h)-sum(h)) ) )
  return( uc )
}

u.hs <- uc.stat(p,h.hs)
u.n  <- uc.stat(p,h.n)
u.st <- uc.stat(p,h.st)
u.np <- uc.stat(p,h.np)


print( round( 1-pchisq(c(u.hs,u.n,u.np,u.st),1) , 3 ) )

# The Dynamic Quantile Test
# The objective of this Test is to check whether we out Hit at time t is independent 
# of the hits in t-q hits, where q is an arbitrary number that we pick.
# In the following example, we assume that the value of k is 5
# The function dq stat takes as input the p-value and the hits and outputs the 
# dq stat. 

# If the p-value is significant, then the hits are independent. If it is not 
# significant, then the hits are dependent. 

dq.stat <- function(p,h){  
  n    <- length(h)
  y    <- h[6:n]-p
  X    <- cbind( h[5:(n-1)] , h[4:(n-2)] , h[3:(n-3)] , h[2:(n-4)] , h[1:(n-5)] )
  b    <- solve( t(X) %*% X ) %*% t(X) %*% y
  stat <- (t(b) %*% t(X) %*% X %*% b)/(p*(1-p))
  pval <- 1-pchisq(stat,5)
  return(pval)
}

dq.hs <- dq.stat(p,h.hs)
dq.n  <- dq.stat(p,h.n)
dq.st <- dq.stat(p,h.st)
dq.np <- dq.stat(p,h.np)

print( round( c(dq.hs,dq.n,dq.np,dq.st) , 3 ) )
