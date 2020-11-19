TEMP <- read.csv('A_TEMP_R.csv')
#temp_X1 <- TEMP$X1
#temp_X1[which(temp_X1 == -99999)] <- NA

TEMP_tbl <- as_tibble(TEMP)

x1_x2 <- TEMP_tbl %>%
  select(Time,X1,X2)%>%
  filter(X1!=-99999,X2!=-99999) %>%
  mutate(year_month=substr(Time,1,7),year= substr(Time,1,4),month=substr(Time,6,7))%>% 
  mutate(month_new= ifelse(substr(Time,7,7)!='/',month,paste(0,substr(month,1,1),sep = ''))) %>%
  mutate(year_month_new = paste(year,month_new,sep = ''))%>%
  group_by(year_month_new)%>%
  summarise(X1,X2,X1_month_mean=mean(X1),X2_month_mean = mean(X2),Time) # ѡȡ??ƽ??


#boxplot
ggplot(x1_x2,aes(x=year_month_new,y=X1 ,fill=year_month_new)) +
  geom_boxplot() +
  theme_classic()

x1_x2_uni <-unique( x1_x2 %>%
  select(year_month_new,X1_month_mean,X2_month_mean))

#Time series
x1_time <- ts(x1_x2_uni$X1_month_mean, start=c(2014,1), frequency=12)
plot(x1_time, type="l",col='blue', main='the month mean temp of X1 station',xlab = 'Time',
     ylab = 'month mean temp') 

temp_components <- decompose(x1_time)
plot(temp_components)

#Histogram
hist(x1_x2_uni$X1_month_mean,     
     xlab = "Monthly temp of station X1",
     main = "Histogram of Monthly temp",
     breaks = 20,
     col = "blue",
     border = "red")
box(lwd=2,col="green")

#Scatterplots
c
X1_temp <- TEMP_tbl %>%
  select(Time,X1)%>%
  filter(X1 != -99999)

  
  
plot(X1 ~ as.Date(Time), data = X1_temp,
     xlab = "year_month",
     ylab = "temp",
     main = "monthly temp of station X1",
     pch = "+",
     cex = 2,
     col = "navy")


# image plot
ex.nc     <- open.nc("air.mon.ltm.nc")

# Read the variables
# Lat
Lat       <- var.get.nc(ex.nc, "lat")
# Lon
Lon       <- var.get.nc(ex.nc, "lon")
# Monthly long term mean, surface temperature [degC]
Air_T     <- var.get.nc(ex.nc, "air") 

# Close the NetCDF file
close.nc(ex.nc)

# Original Lat is in decreasing order, we need to reverse it
Lat <- rev(Lat)

# Data transformation of Air_T_Jan
Air_T_Jan <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  Air_T_Jan[,row] <- Air_T[, (length(Lat)+1-row),1 ]
}

par(mar=c(4.5,3,2,1))

# Plot
image.plot(Lon, Lat, Air_T_Jan,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [degC]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long term (1800-2020) mean surface temperature in Jan."),
      cex.main=1,font.main=2)

# Add map
map('world',add=T,lwd=0.75,col="black")

# Add a box
box(lwd=2)




