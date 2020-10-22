library(dplyr)
library(ggplot2)
library(tidyr)

data_shenzhen <- read.csv('2281305.csv')
data_wind <- as_tibble(data_shenzhen)  

data_wind_new = data_wind %>%    
  select(DATE,WND) %>%   #get a data include the date and WND
  #split and get various kinds of data about wind,store them separately
  mutate(wind_1 = as.numeric(substr(WND,1,3)),wind_2 = substr(WND,5,5),wind_3 = substr(WND,7,7),
         wind_4 = as.numeric(substr(WND,9,12)),wind_5 = substr(WND,14,14)) %>% 
  # clean the data, using ifelse to replace the useless data into NA
  mutate(date=as.Date(DATE),month_year=substr(DATE,1,7),year=substr(DATE,1,4) ,wind_1_new =ifelse(wind_1>0 & wind_1 <360,wind_1,NA),
         wind_2_new =ifelse(wind_2 =='1',wind_2,NA),wind_3_new=ifelse(wind_3=="N",wind_3,NA),
         wind_4_new =ifelse(wind_4>0 &wind_4<900,wind_4,NA),wind_5_new=ifelse(wind_5=='1',wind_5,NA)) %>%
  
  group_by(month_year) %>%
  # get a data only include date and month_mean_value
  summarise(date,month_year,month_mean=mean(wind_4_new,na.rm = TRUE)) 
  
data_wind_new %>%                 #make a plot
  ggplot(aes(date,month_mean)) + 
  geom_line() 
  


  
  
  
  
