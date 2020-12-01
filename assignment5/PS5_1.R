library(sp)
library(sf)
library(tiff)
library(raster)
library(rgdal)

# 1.1 load data
#文件存放目录
file_dir <- '/Users/still/Documents/data_for_assignment5'
# 三种类型文件的目录
prec_dir <- 'wc2.1_2.5m_prec'
srad_dir <- 'wc2.1_2.5m_srad'
wind_dir <- 'wc2.1_2.5m_wind'

# 三种类型文件夹下的所有文件名称
docs_prec <- dir(paste(file_dir,prec_dir,sep= '/'))
docs_srad <- dir(paste(file_dir,srad_dir,sep = '/'))
docs_wind <- dir(paste(file_dir,wind_dir,sep = '/'))

#三种类型的文件的所有文件绝对路径
dir_prec <- c()
for(i in prec_dir){
  dir_prec = c(dir_prec , paste(file_dir,prec_dir,docs_prec,sep = '/'))
}

dir_srad <- c()
for(i in srad_dir){
  dir_srad = c(dir_srad , paste(file_dir,srad_dir,docs_srad,sep = '/'))
}

dir_wind <- c()
for(i in wind_dir){
  dir_wind = c(dir_wind , paste(file_dir,wind_dir,docs_wind,sep = '/'))
}


#China_map
China_map <- readOGR("China_map", "bou2_4p") 

# precipitation
prec_sum <- 0
for (i in dir_prec){
  raster_prec <- raster::raster(i)
  raster_prec_crop <- crop(raster_prec, China_map)
  prec_sum <- prec_sum + raster_prec_crop 
}
prec_mean <- prec_sum/12

#plot
prec_mean_china <- mask(prec_mean,China_map)
plot(prec_mean_china,main='prec month_average in China')

# solar radiation
srad_sum <- 0
for (i in dir_srad){
  raster_srad <- raster::raster(i)
  raster_srad_crop <- crop(raster_srad, China_map)
  srad_sum <- srad_sum + raster_srad_crop 
}
srad_mean <- srad_sum/12

#plot
srad_mean_china <- mask(srad_mean,China_map)
plot(srad_mean_china,main='solar radiation month_average in China')


# wind
wind_sum <- 0
for (i in dir_wind){
  raster_wind <- raster::raster(i)
  raster_wind_crop <- crop(raster_wind, China_map)
  wind_sum <- wind_sum + raster_wind_crop 
}
wind_mean <- wind_sum/12

#plot
wind_mean_china <- mask(wind_mean,China_map)
plot(wind_mean_china,main='solar radiation month_average in China')


