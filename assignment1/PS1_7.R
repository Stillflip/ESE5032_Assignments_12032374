#7.1,load and clean data
so2_data <- read.csv(file = 'AQ_SO2-20160101-20161231.csv',header = T)
colnames(so2_data)
head(so2_data)
data_annan <- so2_data $ANNAN
data_710 <- so2_data $XCNAQ710
data_968 <- so2_data $XCNAQ968
data_annan[which(data_annan==-99999)] <- NA
data_710[which(data_710==-99999)] <- NA
data_968[which(data_968==-99999)] <- NA

#7.2 Plot the time series of a certain variable.

time_data <- so2_data $Time
time_data1 <- as.Date(time_data)
plot(time_data1,data_710,col='blue',type='l')


# 7.3 Conduct at least 5 simple statistical checks 


data_710_new <- na.omit(data_710)
num <- length(data_710_new)           #可用值的数量
data_710_mean <- mean(data_710_new)   #数据的平均值
print(data_710_mean)                  
variance <- var(data_710_new)         #数据的方差
std <- sd(data_710_new)               #数据的标准差


#Z检验
z.test(x=data_710_new,alternative = 'two.sided',mu =data_710_mean ,sigma.x = std,conf.level = 0.95)

#t检验
t.test(x=data_710_new,alternative = c('two.sided','less','greater'),mu = data_710_mean,sigma.x = std,conf.level = 0.95)

#威尔科克森符号秩检验
wilcox.test(x= data_710_new,mu=data_710_mean)

#卡方检验
chisq.test(x=data_710_new)

#相关性检验
cor.test(data_710_new,log(data_710_new),alternative = c('two.sided','less','greater'),conf.level = 0.95)


