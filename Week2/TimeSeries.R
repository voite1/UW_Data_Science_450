## Oct 12, 2015 W.Taam
#

# Set working directory
setwd("/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_450/Week2")

## input data from PC
dd <- read.csv('monthly_avg_gas_usage_jan71_oct79.txt', header=F, sep='', strip.white=T )

## sanity check on the input
dd

## string out the data in a vector
dat <- c( t( as.matrix( dd ) ) )

## drop missing during read
dat <- dat[-c(107,108)]

#visualize the raw data as it is
plot(dat, type='b', ylab='avg mthly gas usage', xlab='month index')

## underfitting: fit a straight line regression to the gas usage
fit0 <- lm( dat~ c(1:106) )

## naive approach: find monthly usage by averaging over years of the same month
fit1 <- apply( dd, 2, mean, na.rm=T )

## find the ACF and PACF of the raw data to get a sense of the correlation over time
acf(dat[c(1:106)])
pacf(dat[c(1:106)])
## correlation shows sign of periodicity of 6

## (1,0,0)(0,1,0)6
fit <- arima( dat, order=c(1,0,0),seasonal=list(order=c(0,1,0),period=6))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit autocorrelations at periodic lags

## (0,0,1)(0,1,0)12
fit <- arima( dat, order=c(0,0,1),seasonal=list(order=c(0,1,0),period=12))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit autocorrelations at periodic lags

## (0,0,1)(0,1,1)12
fit <- arima( dat, order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit nearly zero autocorrelations at high lags

## (0,0,1)(1,1,0)12
fit <- arima( dat, order=c(0,0,1),seasonal=list(order=c(1,1,0),period=12))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit nearly zero autocorrelations at high lags

## (1,0,0)(1,1,0)12
fit <- arima( dat, order=c(1,0,0),seasonal=list(order=c(1,1,0),period=12))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit nearly zero autocorrelations at high lags

## (1,0,0)(0,1,1)12
fit <- arima( dat, order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)]) ## residuals exhibit nearly zero autocorrelations at high lags

## solely rank the models by Akaike information criteria AIC
AIC(
  arima( dat, order=c(1,0,0),seasonal=list(order=c(1,0,0),period=6)),
  arima( dat, order=c(1,0,0),seasonal=list(order=c(0,1,0),period=6)),
  arima( dat, order=c(0,0,1),seasonal=list(order=c(0,1,0),period=12)),
  arima( dat, order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12)),
  arima( dat, order=c(0,0,1),seasonal=list(order=c(1,1,0),period=12)),
  arima( dat, order=c(1,0,0),seasonal=list(order=c(1,1,0),period=12)),
  arima( dat, order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
)

## make month labels
tmp <- expand.grid( c(1971:1980), c('01','02','03','04','05','06','07','08','09','10','11','12'))
yyyymm <- sort( paste(tmp[,1],tmp[,2],sep='-') )

## show forecast from final model with the forecast
plot(dat, type='b', ylab='avg mthly gas usage', xlab='month', xlim=c(1,114), xaxt='n' )
lines( c(107:114), unlist(predict(fit,8)[1]), col='red', type='b', pch=20)
axis(1, at=c(1:114), labels=yyyymm[c(1:114)], las=2, cex.axis=0.7)
