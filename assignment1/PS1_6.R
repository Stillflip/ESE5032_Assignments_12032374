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
data_all_filter=data[-del_label]
print(length(data_all_filter))
#获取过滤后的可见性数据（距离）
filter_dis <- list()
filter_num=length(data_vis3)
for (d in 1: filter_num ){
  filter_dis[d] <- as.numeric(substr(data_vis3[d:d],1,6))
}
print(filter_dis)

#获取时间

Obs_Time = data_all_filter$DATE
print(length(Obs_Time))
Obs_Time <- Obs_Time[-del_label]
print(length(Obs_Time))

Obs_Time2=as.Date(Obs_Time)

#绘图
plot(Obs_Time2,filter_dis,lwd=0.5,type='l',col='blue')

# 只有过去三年三年的数据，过去三年的数据里每年都有一个周期性的变化。
#年初能见度的距离数值比较低，然后组建上升年中达到顶峰，之后又下降。
#春冬季节能见度低，夏秋季节比较高。

print(Obs_Time2[1])
2013 %in% as.numeric(substr(Obs_Time2[29000],1,4))
print(filter_dis[29000])
2600 %in% filter_dis[2]

#6.2
vis2010_5 <- 0
vis2010_10 <- 0
vis2010_15 <- 0
vis2010_20 <- 0
vis2010_25 <- 0
vis2010_30 <- 0
vis2010_30_ <- 0

vis2011_5 <- 0
vis2011_10 <- 0
vis2011_15 <- 0
vis2011_20 <- 0
vis2011_25 <- 0
vis2011_30 <- 0
vis2011_30_ <- 0

vis2012_5 <- 0
vis2012_10 <- 0
vis2012_15 <- 0
vis2012_20 <- 0
vis2012_25 <- 0
vis2012_30 <- 0
vis2012_30_ <- 0

vis2013_5 <- 0
vis2013_10 <- 0
vis2013_15 <- 0
vis2013_20 <- 0
vis2013_25 <- 0
vis2013_30 <- 0
vis2013_30_ <- 0

for(p in 1:filter_num){
  
  if(2010 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis2010_5 = vis2010_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis2010_10 = vis2010_10 +1
    
    }else if(filter_dis[p] < 15000){
      vis2010_15 = vis2010_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis2010_20 = vis2010_20 +1
      
    }else if(filter_dis[p] < 25000){
      vis2010_25 = vis2010_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis2010_30 = vis2010_30 +1
      
    }else {
      vis2010_30_ = vis2010_30_ +1
    
    }
    
  }else if(2011 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis2011_5 = vis2011_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis2011_10 = vis2011_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis2011_15 = vis2011_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis2011_20 = vis2011_20 +1
      
      
    }else if(filter_dis[p] < 25000){
      vis2011_25 = vis2011_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis2011_30 = vis2011_30 +1
      
    }else {
      vis2011_30_ = vis2011_30_ +1
      
    }
    
  }else if(2012 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis2012_5 = vis2012_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis2012_10 = vis2012_10 +1
      
    }else if(filter_dis[p] < 15000){
      vis2012_15 = vis2012_15 +1
      
    }else if(filter_dis[p] < 20000){
      vis2012_20 = vis2012_20 +1
      
    }else if(filter_dis[p] < 25000){
      vis2012_25 = vis2012_25 +1
      
    }else if(filter_dis[p] < 30000){
      vis2012_30 = vis2012_30 +1
      
    }else {
      vis2012_30_ = vis2012_30_ +1
      
    }
    
  }else if(2013 %in% as.numeric(substr(Obs_Time2[p],1,4)) == TRUE ){
    if(filter_dis[p] < 5000){
      vis2013_5 = vis2013_5 +1
      
    }else if(filter_dis[p] < 10000){
      vis2013_10 = vis2013_10 +1
        
    }else if(filter_dis[p] < 15000){
      vis2013_15 = vis2013_15 +1

    }else if(filter_dis[p] < 20000){
      vis2013_20 = vis2013_20 +1
        
    }else if(filter_dis[p] < 25000){
      vis2013_25 = vis2013_25 +1
 
    }else if(filter_dis[p] < 30000){
      vis2013_30 = vis2013_30 +1
 
    }else {
      vis2013_30_ = vis2013_30_ +1
    }
    
  }
  
}

#分析能见度质量
#创建堆叠条形图   
#画图代码参考：https://blog.csdn.net/g863402758/article/details/53389564

colors <- c("green","orange","brown",'red','blue','yellow','pink')
years <- c("2010","2011","2012","2013")
regions <- c("vis_5","vis_10","vis_15",'vis_20','vis_25','vis_30','vis_30_')


# Create the matrix of the values.
Values <- matrix(c(vis2010_5,vis2011_5,vis2012_5,vis2013_5,
                   vis2010_10,vis2011_10,vis2012_10,vis2013_10,
                   vis2010_15,vis2011_15,vis2012_15,vis2013_15,
                   vis2010_20,vis2011_20,vis2012_20,vis2013_20,
                   vis2010_25,vis2011_25,vis2012_25,vis2013_25,
                   vis2010_30,vis2011_30,vis2012_30,vis2013_30,
                   vis2010_30_,vis2011_30_,vis2012_30_,vis2013_30_),
                 nrow=7,ncol=4,byrow=TRUE)

# Give the chart file a name.
png(file = "2010-2013堆叠条形图.png")

# Create the bar chart.
barplot(Values,main="visibility of 2010-2013",names.arg=years,xlab="years",ylab="m",col=colors)

# Add the legend to the chart.
legend("topright", regions, cex=1.3, fill=colors)

# Save the file.
dev.off()

#各年度各级能见度数量，见"2010-2013堆叠条形图.png"，因2013年数据量太少因此不进行比较
#visibility getting a little better



