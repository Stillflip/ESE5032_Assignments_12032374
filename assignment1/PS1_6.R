data <- read.csv('2281305.csv')
data_vis <- data$VIS
print(data_vis)
num=length(data_vis)
print(num)
distance <- list()
dis_qua <- list()
vari_code <- list()
qua_vari_code <- list()
del_label <-c()
print(distance[29254])
for(i in 1:num){
  distance[i] <- as.numeric(substr(data_vis[i:i],1,6))
  dis_qua[i] <- substr(data_vis[i:i],8,8)
  vari_code[i] <- substr(data_vis[i:i],10,10)
  qua_vari_code[i] <-substr(data_vis[i:i],12,12)
 
}
print(distance[29254])
for (j in 1:num){
  if (distance[j] <0 | distance[j]>160000 ){ 
    del_label= c(del_label,j)
  }
}
print(length(del_label))

for (t in 1:num){
  if (dis_qua[t] != '1' | vari_code[t] !='N' | qua_vari_code[t] != '1'){ 
    del_label= c(del_label,t)
  }
}
print(length(del_label))    
data_vis3=data_vis[-del_label]   #获取到过滤后的数据

#获取过滤后的可见性数据（距离）
filter_dis <- list()
for (d in 1: length(data_vis3) ){
  filter_dis[d] <- as.numeric(substr(data_vis3[1:1],1,6))
}

#获取时间
BaoAn_T=data['TMP']
Obs_Time = data$DATE
BaoAn_T_value=substr(BaoAn_T[,1],1,5)
BaoAn_T_flag = substr(BaoAn_T[,1],7,7)
print(BaoAn_T_flag)
Obs_Time2=as.Date(BaoAn_T)
BaoAn_T_value2=as.numeric(BaoAn_T_value)
BaoAn_T_value2
BaoAn_T_flag2=as.logical(as.numeric(BaoAn_T_flag))
BaoAn_T_flag2
BaoAn_T_value2[which(BaoAn_T_value2==9999)] <-NA
BaoAn_T_value2
BaoAn_T_value3=BaoAn_T_value2*0.1
BaoAn_T_value3
Obs_Time2=as.Date(Obs_Time)

#绘图
plot(Obs_Time2,filter_dis,lwd=0.5,type='l',col='blue')
