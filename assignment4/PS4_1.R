TEMP <- read.csv('A_TEMP_R.csv')
#temp_X1 <- TEMP$X1
#temp_X1[which(temp_X1 == -99999)] <- NA

TEMP_tbl <- as_tibble(TEMP)

x1_x2 <- TEMP_tbl %>%
  select(Time,X1,X2)%>%
  filter(X1!=-99999,X2!=-99999) %>%
  mutate(year_month=substr(Time,1,7),year= substr(Time,1,4),month=substr(Time,6,7),day = )%>% 
  mutate(month_new= ifelse(substr(Time,7,7)!='/',month,paste(0,substr(month,1,1),sep = ''))) %>%
  mutate(year_month_new = paste(year,month_new,sep = ''))%>%
  group_by(year_month_new)%>%
  summarise(X1,X2,X1_month_mean=mean(X1),X2_month_mean = mean(X2),Time) # ѡȡ??ƽ??


#boxplot
ggplot(x1_x2,aes(x=year_month_new,y=X1 ,fill=year_month_new)) +
  geom_boxplot() +
  theme_classic()

x1_x2_uni <-unique( x1_x2 %>%
  select(year_month_new,X1_month_mean,X2_month_mean))

#Time series
x1_time <- ts(x1_x2_uni$X1_month_mean, start=c(2014,1), frequency=12)
plot(x1_time, type="l",col='blue', main='the month mean temp of X1 station',xlab = 'Time',
     ylab = 'month mean temp') 

temp_components <- decompose(x1_time)
plot(temp_components)

#Histogram
hist(x1_x2_uni$X1_month_mean,     
     xlab = "Monthly temp of station X1",
     main = "Histogram of Monthly temp",
     breaks = 20,
     col = "blue",
     border = "red")
box(lwd=2,col="green")

#Scatterplots

X1_temp <- TEMP_tbl %>%
  select(Time,X1)%>%
  filter(X1 != -99999)

  
  
plot(X1 ~ as.Date(Time), data = X1_temp,
     xlab = "year_month",
     ylab = "temp",
     main = "monthly temp of station X1",
     pch = "+",
     cex = 2,
     col = "navy")

# image plot




