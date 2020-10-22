data <- read.csv('AQ_SO2-20160101-20161231.csv')
data1 <- as_tibble(data)
data1
data1 %>%
  mutate(date = as.Date(Time) ,annan_new= ifelse(ANNAN != -99999.0,ANNAN,NA),
         new_710= ifelse(XCNAQ710 != -99999.0,XCNAQ710,NA),
         new_968= ifelse(XCNAQ968 != -99999.0,XCNAQ968,NA)) %>%
  ggplot(aes(x=date,y=new_710)) +
  geom_line()
  

