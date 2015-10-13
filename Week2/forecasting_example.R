setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_450/Week2')
file <- 'monthly_avg_gas_usage_jan71_oct79.txt'
dd <- read.csv(file, sep = "", header=F, strip.white=T)
dd
dat <- c(t(as.matrix(dd)))

acf(as.ts(na.omit(dat)))
# or
acf(as.ts(dat[c(1:106)]))

plot(dat, type='b')

monthplot(dat)

# Basic moving average is the line below
mavg <- apply(dd, 2, mean, na.rm=T)
mavg

lm(dat ~ c(1:108))

# using ARIMA
fit <- arima(dat, order=c(0,0,1), seasonal=list(order=c(1,0,0), period=6))
acf(fit$residuals[c(1:106)])
pacf(fit$residuals[c(1:106)])
