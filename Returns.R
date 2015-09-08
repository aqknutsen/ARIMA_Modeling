#Load All packages 
library(quantmod)
library(forecast, quietly = T)
library(tseries, quietly = T)

# PUT STOCK HERE
getSymbols("WMT")

# Store Returns


temp = WMT[];
price <- temp[,6]

temp1 = diff(price)
returns = temp1[2:length(temp1)]

num = 1:length(returns)
nums=1:365

lm.r=lm(returns~num)



#Check if returns are stationary with a kpss.test
# At 5% level significance if p < 0.05 the trend is non-stationary
# If p > 0.05 the trend is stationary 
# Returns should be stationary  
kpss.test(returns)



# Uses auto.arima to develop a ARIMA Model for returns we know difference = 1
fit <-auto.arima(returns,approximation=FALSE,trace=FALSE)

#Dipslay the Summary of the Fit
summary(fit)

lm.r1=lm(residuals(fit)~num)

arma.forecast <- forecast(fit, h = 365)


pdf("C:/Users/Alec/Documents/R/Bozenna_Research/Stocks/Returns_Analysis.pdf")
#Plot Returns, histograms of Returns, and Summary of returns to see if normalized - SHOULD NOT BE NORMAL
plot(returns, main="Returns vs. Time")

hist(returns,main = "Histograms of Returns")

plot(lm.r, main = "Returns vs. Time")

#Display the nonstationary returns with ACF and PACF
tsdisplay(returns,main="Returns vs. Time")

#Plot the residuals. Make sure they look like white noise (normal with mean 0 and constant variance)
#Plot residuals and make sure they look like white noise.
plot(residuals(fit), main = "Residuals for ARIMA for PRICE")


plot(lm.r1,main="Residuals for ARIMA for PRICE")

#Forecast Future Predictions for rest of year

plot(arma.forecast,main="ARMA Future Predictions for Returns FOR NEXT YEAR")
dev.off()

print(arma.forecast)

