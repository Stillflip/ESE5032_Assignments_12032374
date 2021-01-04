library(dplyr)
library(tibble)
library(ggplot2)
library(gapminder)

all_data <- read.csv('all_data.csv')
all_tbl <- as_tibble(all_data)

all_use <- all_tbl %>%
  select(t2m,d2m,blh,sp,e,tp,fal,u10,v10,wind_direction,wind_speed,bc)

plot(all_use,main='Matrix Scatterplot') 
 
model <-lm(bc ~ t2m + d2m + e + fal + wind_direction + wind_speed + blh + sp + tp, data = all_use)
coef(model)
summary(model)


gapminder %>%
  dplyr::filter( continent=="Asia" | continent=="Europe" ) %>%   
  dplyr::filter( substr(country,1,1)=="C" ) %>% 
  ggplot( aes(x=year, y=gdpPercap) ) + 
  geom_point()
print(gapminder)
