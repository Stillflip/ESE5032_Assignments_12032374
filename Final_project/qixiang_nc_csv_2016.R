library(RNetCDF)
library(dplyr)

jjj_2016_qixiang <- open.nc('jjj_2016_all.nc')
print.nc(jjj_2016_qixiang)

time_jjj <- var.get.nc(jjj_2016_qixiang,"time") -1016832  # time from 2016.01.01.00

qixiang_vari_2016<- c('u10','v10','t2m','d2m','blh','e','sp','tp','fal')
for (i in 1:9) {
  assign(paste(qixiang_vari_2016[i],'_2016_miyun',sep = ''), var.get.nc(jjj_2016_qixiang, qixiang_vari_2016[i],unpack = TRUE)[12,19,])
  assign(paste(qixiang_vari_2016[i],'_2016_changpin',sep = ''), var.get.nc(jjj_2016_qixiang, qixiang_vari_2016[i],unpack = TRUE)[14,16,])
  assign(paste(qixiang_vari_2016[i],'_2016_beijing',sep = ''),  var.get.nc(jjj_2016_qixiang, qixiang_vari_2016[i],unpack = TRUE)[16,17,])
  assign(paste(qixiang_vari_2016[i],'_2016_daxing',sep = ''),  var.get.nc(jjj_2016_qixiang, qixiang_vari_2016[i],unpack = TRUE)[17,16,])
  
}
#温度转换为度
t2m_2016_beijing <- t2m_2016_beijing-273.15
t2m_2016_daxing <- t2m_2016_daxing-273.15
t2m_2016_changpin <- t2m_2016_changpin-273.15
t2m_2016_miyun <- t2m_2016_miyun-273.15
d2m_2016_beijing <- d2m_2016_beijing-273.15
d2m_2016_daxing <- d2m_2016_daxing-273.15
d2m_2016_changpin <- d2m_2016_changpin-273.15
d2m_2016_miyun <- d2m_2016_miyun-273.15



#miyun area
miyun_vari <-t(rbind(time_jjj,t2m_2016_miyun,d2m_2016_miyun,u10_2016_miyun,v10_2016_miyun,blh_2016_miyun,
                     sp_2016_miyun,e_2016_miyun,tp_2016_miyun,fal_2016_miyun))
miyun_vari_tbl <- as_tibble(miyun_vari)

#将u_wind和v_wind转换为speed和direction
miyun_vari_all <- miyun_vari_tbl %>%
  mutate(wind_direction = atan(u10_2016_miyun/v10_2016_miyun)*180,
         wind_speed = abs(u10_2016_miyun/sin(wind_direction/180) ))
write.csv(miyun_vari_all,'miyun_2016_all.csv')


#changpin area
changpin_vari <-t(rbind(time_jjj,t2m_2016_changpin,d2m_2016_changpin,u10_2016_changpin,v10_2016_changpin,blh_2016_changpin,
                     sp_2016_changpin,e_2016_changpin,tp_2016_changpin,fal_2016_changpin))
changpin_vari_tbl <- as_tibble(changpin_vari)

#将u_wind和v_wind转换为speed和direction
changpin_vari_all <- changpin_vari_tbl %>%
  mutate(wind_direction = atan(u10_2016_changpin/v10_2016_changpin)*180,
         wind_speed = abs(u10_2016_changpin/sin(wind_direction/180) ))
write.csv(changpin_vari_all,'changpin_2016_all.csv')

#daxing area
daxing_vari <-t(rbind(time_jjj,t2m_2016_daxing,d2m_2016_daxing,u10_2016_daxing,v10_2016_daxing,blh_2016_daxing,
                     sp_2016_daxing,e_2016_daxing,tp_2016_daxing,fal_2016_daxing))
daxing_vari_tbl <- as_tibble(daxing_vari)

#将u_wind和v_wind转换为speed和direction
daxing_vari_all <- daxing_vari_tbl %>%
  mutate(wind_direction = atan(u10_2016_daxing/v10_2016_daxing)*180,
         wind_speed = abs(u10_2016_daxing/sin(wind_direction/180) ))
write.csv(daxing_vari_all,'daxing_2016_all.csv')



