library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)

baoan <- read.csv('2281305.csv')
baoan_tbl <- as_tibble(baoan)
tem_time <- baoan_tbl %>%
  mutate(month = substr(DATE,6,7),year_month=substr(DATE,1,7),
         temp_new = ifelse(substr(TMP,7,7)==1,substr(TMP,2,5), NA)) %>%
  group_by(year_month) %>%
  summarize(temp_mean=mean(as.numeric(temp_new), na.rm = TRUE))

temp_year <- ts(tem_time$temp_mean, start=c(2010,1), frequency=12)
plot(temp_year, type="l", col='red')

#2.2
temp_components <- decompose(temp_year)
plot(temp_components,col='blue')

hist(temp_components$random, prob=TRUE,col = "blue",border = "yellow")
# Add pdf
curve(dnorm(x, mean=mean(temp_components$random,na.rm=T),
            sd=sd(temp_components$random,na.rm=T)),
      add=TRUE, col="red")


#2.3 AR Model
# we can see that the data has trend, so it is not a Stationary time series
#so ,we need to do the difference operation

#因为没有2020年10月份的真实数据，所以取2010年1月份-2019年4月份的数据进行拟合，
#来预测2020年5月至9月的数据并使用真实值进行评估
temp_for_fit <- ts(tem_time$temp_mean[1:124], start=c(2010,1), frequency=12)
plot(temp_for_fit)

temp_year_log = log(temp_for_fit)
plot(temp_year_log,col='blue')

# Check acf and pacf
acf(temp_year_log)
pacf(temp_year_log)

# Take the diff, d=1
temp_year_log_d1 <- diff(temp_year_log)

# Plot time series
plot(temp_year_log_d1,col='blue')

# Check acf and pacf
acf(temp_year_log_d1,col='blue')
pacf(temp_year_log_d1,col='blue')

# Automated forecasting using an ARIMA model
model <- auto.arima(temp_year_log)
summary(model)

#2.4 predict
# Make predictions 
forecast_5months <- forecast(model, 5)
plot(forecast_5months)


# Plot predictions along with real values
plot(forecast(model, 5), include = 30, xlab="Time", 
     ylab="log(temp_year_log_d1)",type="o",lwd=2) 

# Get predicted values

# get the value of the latter 5 months
day_forward <- 5
predict_mean_values <- exp(forecast_5months$mean[day_forward])
predict_values <- exp(forecast_5months$mean)
exp(forecast_5months$mean[day_forward])
exp(forecast_5months$lower[day_forward,1])
exp(forecast_5months$upper[day_forward,1])



# Verify the predictions
real_values <- tail(tem_time)
mean_values <- mean(real_values$temp_mean[2:6])
bias = (mean_values - predict_mean_values)/mean_values
print(bias)


bias_every <- (real_values$temp_mean[2:6] - predict_values)/real_values$temp_mean[2:6]

#mean_bias : 0.000226408
#每个月份的误差
# May          Jun          Jul          Aug          Sep
# 2020  0.042162860  0.020763919  0.011700228 -0.020018560  0.007128324

# predict values
# forecast_5months
#          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# May 2020       5.585117 5.505395 5.664839 5.463193 5.707041
# Jun 2020       5.660167 5.577796 5.742539 5.534191 5.786144
# Jul 2020       5.703099 5.619204 5.786993 5.574793 5.831404
# Aug 2020       5.700424 5.616530 5.784319 5.572118 5.828730
# Sep 2020       5.678195 5.594301 5.762090 5.549890 5.806501

#         May      Jun      Jul      Aug      Sep
# 2020 266.4315 287.1967 299.7949 298.9942 292.4212

# real_value

# 1 2020-04         218.
# 2 2020-05         278.
# 3 2020-06         293.
# 4 2020-07         303.
# 5 2020-08         293.
# 6 2020-09         295.



