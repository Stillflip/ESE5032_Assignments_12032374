library(dplyr)
library(ggplot2)
library(tidyr)

#1.1
Sig_Eq <- read.delim(file = "signif.txt",header = TRUE)  #read the data
head(Sig_Eq)
Sig_Eqs <- as_tibble(Sig_Eq)   #convert it to a tibble object 
Sig_Eqs

#1.2

death_country <- Sig_Eqs %>%
  select(COUNTRY,DEATHS) %>%
  group_by(COUNTRY) %>%
  #count the total number of death in each country
  summarise(total_death = sum(DEATHS,na.rm = TRUE)) %>% 
  arrange(desc(total_death))  # arrange them from big to small

death_country
death_country[1:10,]


#1.3
Sig_Eqs %>%
  select(YEAR,EQ_PRIMARY) %>%
  group_by(YEAR) %>%
  summarise(mag_6.0 = sum(EQ_PRIMARY>6.0, na.rm= TRUE)) %>% #the sum of magnitude larger than 6.0
  ggplot(aes(YEAR,mag_6.0)) +
  geom_line()


#1.4

CountEq_LargestEq <- function(country){
  co_eq = Sig_Eqs %>%
    select(COUNTRY,EQ_PRIMARY) %>%  #pick the data that needed
    filter(COUNTRY == country)      # filter the country which is inputted
  num_eq = as.numeric(nrow(co_eq))    # country's number of earthquake
  
  date_cou = Sig_Eqs %>%
    filter(COUNTRY == country) %>%   # filter the country which inputed
    select(YEAR,MONTH,DAY,COUNTRY,EQ_PRIMARY) %>%   #pick the data that needed
    #creat the date variable
    mutate(date=paste(as.character(YEAR),'year', as.character(MONTH),'month',as.character(DAY),'day')) %>%
    arrange(desc(EQ_PRIMARY)) # arrange it in sequence
  # in the case that the country have more than one biggest earthquake.
  date_country = date_cou$date[(date_cou$EQ_PRIMARY == max(date_cou$EQ_PRIMARY,na.rm = TRUE )) ]
  # delete the NA parts
  date_country=date_country[!is.na(date_country)]
  
  #return the number of earthquake, and the date that happen
  return(c(num_eq, date_country))
    
}

CountEq_LargestEq('GREECE')

country_all = unique(Sig_Eqs$COUNTRY)

for(i in country_all){
  print(i)                     #print every country's name
  print(CountEq_LargestEq(i))  #print the return of the function above
  
}





  