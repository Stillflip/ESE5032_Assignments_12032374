library(dplyr)
library(tibble)


changpin <- read.csv('changpin.csv')
changpin_tbl <- as_tibble(changpin)

changpin_use <- changpin_tbl %>%
  mutate(wind_direction = atan(u10_changpin/v10_changpin)*180,
         wind_speed = abs(u10_changpin/sin(wind_direction/180) ))%>%
  mutate(month=as.numeric(substr(as.character(time),5,6)),day=as.numeric(substr(as.character(time),7,8)))%>%
  select(time_jjj,month,day,u10_changpin,v10_changpin,t2m_changpin,wind_direction,wind_speed,blh_changpin,lai_hv_changpin,lai_lv_changpin,
         str_changpin,sp_changpin,sshf_changpin,ttr_changpin,tp_changpin,bc_changpin)

plot(changpin_use,main='Matrix Scatterplot') 
 

model <-lm(bc_changpin ~ t2m_changpin + wind_direction + wind_speed + blh_changpin + lai_hv_changpin + lai_lv_changpin + str_changpin + sp_changpin + sshf_changpin + ttr_changpin + tp_changpin, data = changpin_use)
coef(model)
summary(model)


model1 <-lm(bc_changpin ~ t2m_changpin + wind_direction + wind_speed + blh_changpin + sp_changpin + tp_changpin, data = changpin_use)
coef(model1)
summary(model1)

model2 <-lm(bc_changpin ~ t2m_changpin + u10_changpin + v10_changpin + blh_changpin + sp_changpin + tp_changpin, data = changpin_use)
coef(model2)
summary(model2)
