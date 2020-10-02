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

for (j in 1:num){
  if (dis_qua[j] != '1' | vari_code[j] !='N' | qua_vari_code[j] != '1'){ 
    del_label= c(del_label,j)
  }
}

if (distance[i] <0 | distance[i]>160000 | dis_qua[i] != '1' | vari_code[i] !='N' | qua_vari_code[i] != '1'){ 
  del_label= c(del_label,j)
}

print(length(del_label))


data_vis2=data_vis[del_label] <-NA
print(length(data_vis2))

if(dis_qua[i] != '1' | vari_code[i] !='N' | qua_vari_code[i] != '1'){
  del_label = c(c,i)
}

