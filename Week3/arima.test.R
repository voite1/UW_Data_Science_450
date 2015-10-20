# Beer example

# Set working directory
setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_450/Week3')

# Read csv
beer <- read.csv('beer.csv')

# Create time seriece
beer.ts <- ts(beer, start=c(1956,1), frequency = 12)

# Print beer.ts
beer.ts

# Plot beer time series
plot(beer.ts)

# Smooth the beer.ts line using lowess function
plot(lowess(beer.ts, f = 0.05), type = "l")

# Install and load 'forecast' package
install.packages("forecast")
library(forecast)

# Use forecasst
beer.forecast <- forecast(beer.ts, h =16, level = 0.95)

# Plot prection vs. original smoothed line (red)
plot(beer.forecast)
lines(lowess(beer.ts, f = 0.05), type="l", col="red")


##########################################################
### Using auto.arima(beer.ts)
auto.arima(beer.ts)  # returned ARIMA(1,1,0)(2,0,0)[12]

# run arima() using parameters produced by auto.arima
beerfit2 <- arima(beer.ts, order=c(1,1,0), seasonal = list(order=c(2,0,0), periods = 12))

# create prediction for beers
beerpred2 <- predict(beerfit2, n.ahead=16)

# print out beer predictions
beerpred2

# look at attributes of beerpred2
attributes(beerpred2)

# print 
predict(beer.forecast)
