library(tiff)
library(raster)
library(rgdal)
file_dir <- 'D:/GitKraken/ESE5032_Assignments_12032374/assignment5'
read_file <- readTIFF('wc2.1_2.5m_prec_01.tif')


image.plot(read_file)


# 三种类型文件的目录
prec_dir <- 'wc2.1_2.5m_prec'
srad_dir <- 'wc2.1_2.5m_srad'
wind_dir <- 'wc2.1_2.5m_wind'

# 三种类型文件夹下的所有文件名称
docs_prec <- dir(prec_dir)
docs_srad <- dir(srad_dir)
docs_wind <- dir(wind_dir)

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


