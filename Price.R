library(XLConnect)
library(MASS)
library(forecast, quietly = T)
library(tseries, quietly = T)
library(seasonal)
library(quantmod)

# PUT STOCK HERE
getSymbols("AMBA")

temp <-AMBA['2010-01-01::2015-01-01']
price <- temp[,6]


timeseries = ts(as.numeric(price),freq=length(price)/5,start=c(2010,1))



adjusted = seasadj(stl(timeseries,s.window="periodic"))


windows()
plot(stl(timeseries,s.window="periodic"),main="Plot of seasonal adjustment")
windows()
plot(timeseries,main="Plot of Original Time Series with Seasonally adjusted")
par(new = TRUE)
plot(adjusted,col="red",xaxt="n",yaxt="n",ylab=NULL,xlab=NULL)
par(new=TRUE)



lambda<-BoxCox.lambda(adjusted)
print(lambda)
series_trans <-ts(as.numeric(adjusted),freq=length(adjusted)/5,start=c(2010,1))


for(i in 1:length(adjusted)) {

	series_trans[i] = ((adjusted[i]^(lambda)) - 1 )/lambda

}

first_diff = ts(as.numeric(diff(adjusted)),freq=length(adjusted)/5,start=c(2010,1))

windows()
plot(series_trans, main = "Plot of Seasonally Adjusted with BOX-Cox Transformation")
kpss.test(series_trans)



windows()
plot(first_diff, main="Plot of First Difference of Seasonally adjusted")
kpss.test(first_diff)



windows()
tsdisplay(first_diff,main = "ACF,PACF for Seasonlly adjusted, FirstDifference")
fit <-auto.arima(as.numeric(adjusted),approximation=FALSE,trace=FALSE)
summary(fit)
arma.forecast <- forecast(fit, h = length(first_diff)/5)

windows()
plot(residuals(fit), main = "Residuals for fitted ARIMA for Seasonly Adjusted,First Difference")



windows()
plot(arma.forecast, main="ARMA Future Predictions for Seasonly Adjusted, First Difference")

windows()
tsdisplay(residuals(fit))

Box.test(residuals(fit),lag=length(residuals(fit))/5,fitdf=7,type="Ljung")




