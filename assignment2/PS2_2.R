data_shenzhen <- read.csv('2281305.csv')
data_wind <- as_tibble(data_shenzhen)

data_wind_new = data_wind %>%
  select(DATE,WND) %>%
  mutate(wind_1 = as.numeric(substr(WND,1,3)),wind_2 = substr(WND,5,5),wind_3 = substr(WND,7,7),
         wind_4 = as.numeric(substr(WND,9,12)),wind_5 = substr(WND,14,14)) %>%
  mutate(date=as.Date(DATE),month=substr(DATE,1,7) ,wind_1_new =ifelse(wind_1>0 & wind_1 <360,wind_1,NA),
         wind_2_new =ifelse(wind_2 =='1',wind_2,NA),wind_3_new=ifelse(wind_3=="N",wind_3,NA),
         wind_4_new =ifelse(wind_4>0 &wind_4<900,wind_4,NA),wind_5_new=ifelse(wind_5=='1',wind_5,NA)) %>%
  
  group_by(month) %>%
  summarise(date,month,month_mean=mean(wind_4_new,na.rm = TRUE)) 
  
data_wind_new %>%
  ggplot(aes(date,month_mean)) + 
  geom_line()
  


  
  
  
  
