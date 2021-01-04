library(fields)
library(maps)
library(RNetCDF)
library(dplyr)
library(tibble)
library(sp)
library(rgdal)

nc_2016 <- open.nc('/Users/still/Downloads/R_final_project/jjj_2016_all.nc')
print.nc(nc_2016)
#经纬度信息
Lat_2016 <- var.get.nc(nc_2016, "latitude")
Lon_2016 <- var.get.nc(nc_2016,"longitude")

print(jjj_u10)
#t2m  units du
jjj_t2m <- var.get.nc(nc_2016, "t2m",unpack = TRUE) - 273.15

#u10 
jjj_u10 <- var.get.nc(nc_2016, "u10",unpack = TRUE)

#v10 
jjj_v10 <- var.get.nc(nc_2016, "v10",unpack = TRUE)

#blh
jjj_blh <- var.get.nc(nc_2016, "blh",unpack = TRUE)

# e
jjj_e <- var.get.nc(nc_2016, "e",unpack = TRUE)

#sp
jjj_sp <- var.get.nc(nc_2016, "sp",unpack = TRUE)

#tp
jjj_tp <- var.get.nc(nc_2016, "tp",unpack = TRUE)

#d2m
jjj_d2m <- var.get.nc(nc_2016,'d2m',unpack = TRUE) - 273.15

# fal
jjj_fal <- var.get.nc(nc_2016,'fal',unpack = TRUE)

qixiang_vari_2016<- c('u10','v10','t2m','d2m','blh','e','sp','tp','fal')
variable_name <- c()
for (i in 1:9) {
  assign(paste(qixiang_vari_2016[i],'_hour',sep = ''), array(var.get.nc(nc_2016, qixiang_vari_2016[i],unpack = TRUE)[10:19,14:21,1],dim=c(80,1)))
  variable_name <- c(variable_name,paste(qixiang_vari_2016[i],'_hour',sep = ''))
  
  
}

data <- cbind(u10_hour,v10_hour,t2m_hour,d2m_hour,blh_hour,e_hour,sp_hour,tp_hour,fal_hour)
write.csv(data,'data_test.csv')

bc_pred <- read.csv('/Users/still/Downloads/R_final_project2/bc_pred.csv',header = TRUE)[,2]
print(bc_pred)

bc_pred_erwei <- array(bc_pred,c(10,8))


China_map <- readOGR("China_map", "bou2_4p") 
#make plot
# Original Lat is in decreasing order, we need to reverse it
Lat <- rev(Lat_2016)

# Data transformation of Air_T_Jan
temp <- array(NA,dim=c(length(Lon_2016[10:19]),length(Lat[14:21])))


for(row in 1:length(Lat[14:21])){
  temp[,row] <- bc_pred_erwei[, (length(Lat[14:21])+1-row)]
}
image.plot(Lon_2016[10:19],Lat[14:21],temp)

# Set margins on bottom, left, top, right
par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon_2016[10:19], Lat[14:21], temp,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="BC estimate /ng",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon_2016),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("BC estimate ."),
      cex.main=1,font.main=2)

# Add map
map(China_map,add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)
