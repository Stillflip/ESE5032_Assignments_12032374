library(dplyr)
library(tibble)


daxing_2016_use <- read.csv('/Users/still/Downloads/R_final_project2/daxing_2016_use.csv')
changpin_2016_use <- read.csv('/Users/still/Downloads/R_final_project2/changpin_2016_use.csv')
miyun_2016_use <- read.csv('/Users/still/Downloads/R_final_project2/miyun_2016_use.csv')

daxing_tbl <- daxing_2016_use%>%
  filter(bc>0)
write.csv(daxing_tbl,'daxing_use.csv')

changpin_tbl <-changpin_2016_use%>%
  filter(bc>0)
write.csv(changpin_tbl,'changpin_use.csv')


miyun_tbl <-miyun_2016_use%>%
  filter(bc>0)
write.csv(miyun_tbl,'miyun_use.csv')


all_data <- rbind(daxing_tbl,changpin_tbl,miyun_tbl)
write.csv(all_data,'all_data.csv')
