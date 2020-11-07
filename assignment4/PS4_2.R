baoan <- read.csv('2281305.csv')
baoan_tbl <- as_tibble(baoan)
tem_time <- baoan_tbl %>%
  mutate(month = substr(DATE,6,7),year_month=substr(DATE,1,7),
         temp_new = ifelse(substr(TMP,7,7)==1,substr(TMP,2,5), NA)) %>%
  group_by(year_month) %>%
  summarize(temp_mean=mean(as.numeric(temp_new), na.rm = TRUE))

temp_year <- ts(tem_time$temp_mean, start=c(2010,1), frequency=12)
plot(temp_year, type="l")

#2.2
temp_components <- decompose(temp_year)
plot(temp_components)

hist(temp_components$random, prob=TRUE)
# Add pdf
curve(dnorm(x, mean=mean(temp_components$random,na.rm=T),
            sd=sd(temp_components$random,na.rm=T)),
      add=TRUE, col="red")


#AR Model
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",
     main=(expression(AR(1)~~~phi==-.9)))

