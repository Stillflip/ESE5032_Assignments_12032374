library(tibble)
library(dplyr)

station_dis <- read.csv('station_dis.csv')
station_tib <- as_tibble(station_dis)

rh_station <- station_tib %>%
  filter(rh_Longitude>115 & rh_Longitude<118 & rh_Latitude>38 & rh_Latitude<41) %>%
  select(station,rh_Latitude,rh_Longitude)


visibility_station <- station_tib %>%
  filter(vis_Longitude>116 & vis_Longitude<117.2 & vis_Latitude>39.4 & vis_Latitude<40.7) %>%
  select(station,vis_Latitude,vis_Longitude)

pm2.5_station <- station_tib %>%
  filter(pm2.5_Longitude>115 & pm2.5_Longitude<118 & pm2.5_Latitude>38 & pm2.5_Latitude<42) %>%
  select(station,pm2.5_Latitude,pm2.5_Longitude)


pm2.5_jjj <-station_tib %>%
  filter(pm2.5_Longitude>112.5 & pm2.5_Longitude<120.5 & pm2.5_Latitude>35.5 & pm2.5_Latitude<43.5) %>%
  select(station,pm2.5_Latitude,pm2.5_Longitude)


#提取beijing范围内的pm2.5站点
station <- paste('X',pm2.5_station$station,sep = '')
pm2.5_vari <- read.csv('pm2.5_data.csv')
pm2.5_tbl <- as_tibble(pm2.5_vari)
pm2.5_beijing <- pm2.5_tbl %>%
  select(station) 
write.csv(pm2.5_beijing,'pm2.5_beijing.csv')

#提取jjj范围内的pm2.5站点

jjj_stations_pm2.5 <- paste('X',pm2.5_jjj$station,sep = '')
pm2.5_jjj_data <- pm2.5_tbl %>%
  select(jjj_stations_pm2.5) 
write.csv(pm2.5_jjj_data,'pm2.5_jjj_data.csv')



#提取离站点10km以内的站点。changping:lat：40.217，lon：116.217
#daxing: lat:39.43,lon:116.21 ; miyun: lat:40.65,lon:117.117


len <- length(station_dis$pm2.5_Latitude)
r <- 0.3
for (i in 1:len) {
  dis = sqrt((station_dis$pm2.5_Latitude[i] - 40.65)^2 + (station_dis$pm2.5_Longitude[i] - 117.117)^2)
  if(dis <= r){
    print(i)
  }
}


pm2.5_data <- read.csv('AQ_FSPMC-20190101-20191231.csv')
pm2.5_tbl <- as_tibble(pm2.5_data)

#提取changping地区的pm2.5数据并完成平均  345,341
changping_pm2.5 <- pm2.5_tbl %>%
  select(Time,'X345','X341')%>%
  mutate(hour = 1:8760)%>%
  mutate(day=hour %/% 24 + 1 ) %>%
  filter(hour%%24 >= 11 & hour%%24 <= 15)

changping_pm2.5_mean <-changping_pm2.5 %>%
  group_by(day)%>%
  mutate(X345_clear = ifelse(X345!=-99999,X345,NA),X341_clear = ifelse(X341!=-99999,X341,NA))%>%
  summarise(X345_pm2.5_mean = mean(X345_clear,na.rm=TRUE),X341_pm2.5_mean = mean(X341_clear,na.rm=TRUE))%>%
  mutate(pm2.5_mean = ifelse(is.na(X345_pm2.5_mean)==FALSE ,X345_pm2.5_mean,X341_pm2.5_mean))
write.csv(changping_pm2.5_mean,'changping_pm2.5_mean.csv')

#提取daxing地区的pm2.5数据并完成平均  332, 561
daxing_pm2.5 <- pm2.5_tbl %>%
  select(Time,'X332','X561')%>%
  mutate(hour = 1:8760)%>%
  mutate(day=hour %/% 24 + 1 ) %>%
  filter(hour%%24 >= 11 & hour%%24 <= 15)

daxing_pm2.5_mean <-daxing_pm2.5 %>%
  group_by(day)%>%
  mutate(X332_clear = ifelse(X332!=-99999,X332,NA),X356_clear = ifelse(X561!=-99999,X561,NA))%>%
  summarise(X332_pm2.5_mean = mean(X332_clear,na.rm=TRUE),X356_pm2.5_mean = mean(X356_clear,na.rm=TRUE))%>%
  mutate(pm2.5_mean = ifelse(is.na(X332_pm2.5_mean)==FALSE ,X332_pm2.5_mean,X356_pm2.5_mean))
write.csv(daxing_pm2.5_mean,'daxing_pm2.5_mean.csv')

#提取miyun地区的pm2.5数据并完成平均  711,330
miyun_pm2.5 <- pm2.5_tbl %>%
  select(Time,'X711','X330')%>%
  mutate(hour = 1:8760)%>%
  mutate(day=hour %/% 24 + 1 ) %>%
  filter(hour%%24 >= 11 & hour%%24 <= 15)

miyun_pm2.5_mean <-miyun_pm2.5 %>%
  group_by(day)%>%
  mutate(X711_clear = ifelse(X711!=-99999,X711,NA),X330_clear = ifelse(X330!=-99999,X330,NA))%>%
  summarise(X711_pm2.5_mean = mean(X711_clear,na.rm=TRUE),X330_pm2.5_mean = mean(X330_clear,na.rm=TRUE))%>%
  mutate(pm2.5_mean = ifelse(is.na(X711_pm2.5_mean)==FALSE ,X711_pm2.5_mean,X330_pm2.5_mean))
write.csv(miyun_pm2.5_mean,'miyun_pm2.5_mean.csv')


