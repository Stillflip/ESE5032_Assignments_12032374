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
for(i in 1:num){
  distance[i] <- as.numeric(substr(data_vis[i:i],1,6))
  dis_qua[i] <- substr(data_vis[i:i],8,8)
  vari_code[i] <- substr(data_vis[i:i],10,10)
  qua_vari_code[i] <-substr(data_vis[i:i],12,12)
  
}
print(distance[29254])
for (j in 1:num){
  if (distance[j] == NA |){ 
    c= c(c,j)
  }
}
print(del_label)
data_vis2=data_vis[which(distance < 0 | distance > 160000 )] <-NA
print(length(data_vis2))



if(dis_qua[i] != '1' | vari_code[i] !='N' | qua_vari_code[i] != '1'){
  del_label = c(c,i)
}