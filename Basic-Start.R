##Time Series R

#Basic Revision
# OLS fit
# When we are trying to fit line, and find out slope and intercept of data cloud no assumption regarding the data point is needed.
#Long Way
data(co2)
plot(co2,main = "Atmospheric concentrations of CO2")
co2.value = as.numeric(co2)
co2.time = as.numeric(time(co2))
value.mean = mean(co2.value) 
time.mean=mean(co2.time)
Sxx = sum((co2.time - time.mean) *  (co2.time - time.mean))
Sxy = sum((co2.value - value.mean) *  (co2.time - time.mean))
slope = Sxy/Sxx
intercept = value.mean - time.mean *  slope
#lazy way
linear_model = lm(co2~time(co2))
linear_model
summary(linear_model)
abline(linear_model)

##Residual
#Long way

residual  = co2.value - (co2.time * slope + intercept)

#short way
resid(linear_model)

plot(residual~time(co2))

#Zoom
plot(residual~time(co2),xlim=c(1970,1975))




## ACF

#Makes a time series 
#lazy way
time_series = ts(rnorm(34))
acf(time_series,type = 'covariance',plot = F)
acf_cov<-vector(mode = "list",length = 15) # lag count is 15
mean.x = mean(time_series)
n = length(time_series)
for(lag_v in 1:15){
  x_lagged = lag(time_series,k=lag_v)
  sxx = 1/n*sum((time_series-mean.x)*(x_lagged-mean.x))
  print(sxx)
  acf_cov[[lag_v]] <- sxx
}

