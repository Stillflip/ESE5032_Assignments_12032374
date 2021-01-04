library(dplyr)
library(tibble)
library(ggplot2)

daxing_use <- read.csv('/Users/still/Downloads/R_final_project2/qixiang_data/daxing_use.csv')
changpin_use <- read.csv('/Users/still/Downloads/R_final_project2/qixiang_data/changpin_use.csv')
miyun_use <- read.csv('/Users/still/Downloads/R_final_project2/qixiang_data/miyun_use.csv')


all_data_station <- rbind(daxing_use,changpin_use,miyun_use)
write.csv(all_data_station,'all_data_station.csv')

all_data_station <- read.csv('/Users/still/Downloads/R_final_project2/all_data_station.csv')
all_tbl <- as_tibble(all_data_station)


all_data_month_mean <- all_tbl %>%
  mutate(ymd = paste(substr(as.character(ymdh),1,4), substr(as.character(ymdh),5,6),
                     substr(as.character(ymdh),7,8),sep='-'),
         month =substr(ymdh,5,6))%>%
  mutate(date = as.Date(ymd))%>%
  group_by(month,Station)%>%
  summarize(Station,t2m_mean = mean(t2m), d2m_mean=mean(d2m),u10_mean= mean(u10),v10_mean = mean(v10),
          blh_mean = mean(blh),sp_mean = mean(sp),tp_mean = mean(tp),e_mean=mean(e),fal_mean= mean(fal),
          wind_direction_mean = mean(wind_direction),wind_speed_mean= mean(wind_speed),bc_mean=mean(bc))
  
all_month_mean <- unique(all_data_month_mean)

all_month_mean %>%
  ggplot(aes(month, bc_mean,color=Station)) +
  geom_point()


#changpin
changpin_mean <- all_day_mean %>%
  filter(Station == 'changping')
  #ggplot(aes(date, bc_mean,color='red')) +
  #geom_point()+
  #labs(title = '                        changpin station BC concentration')
hist(log(changpin_mean$bc_mean),col = 'red')
plot(bc_mean ~ date,data=changpin_mean,col='blue',xlab='2016/Month',ylab = 'concentration /log(ng)')

all_day_mean %>%
  filter(Station == 'daxing')%>%
  ggplot(aes(date, bc_mean, color='blue')) +
  geom_point()+
  labs(title = '                        daxing station BC concentration')


changpin_mean_log <- changpin_mean%>%
  mutate(bc_mean_log = log(bc_mean))
write.csv(changpin_mean_log,'changpin_mean_log.csv')

changpin_log <- changpin_use %>%
  mutate(bc_log = log(bc))
write.csv(changpin_log,'changpin_log.csv')

