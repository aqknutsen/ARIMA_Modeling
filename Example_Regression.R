library(forecast)



mwh <- c(16.3, 16.8, 15.5, 18.2, 15.2, 17.5, 19.8,  19.0,  17.5,  16.0,  19.6,  18.0)

temp <- c(29.3, 21.7, 23.7, 10.4, 29.7, 11.9, 9.0, 23.4, 17.8, 30.0, 8.6, 11.8)

#Plot the energy vs. temperature
windows()
plot(temp, mwh, main ="Energy Consumption vs. Temperature")

fit <-lm(mwh ~ temp)

#Show the summary
print(summary(fit))

#Plot the residuals vs. the temperature 
windows()
plot(temp, residuals(fit), main = "Residuals vs. Temperature")

#Generate fit
my_fit <- forecast(fit, newdata = data.frame(temp = c(10,19)))


#Plot fit
windows()
plot(my_fit, main ="Energy Consumption vs. Temperature")