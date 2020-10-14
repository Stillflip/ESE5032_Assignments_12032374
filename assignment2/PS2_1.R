#1.1
Sig_Eq <- read.delim(file = "signif.txt",header = TRUE)
head(Sig_Eq)
Sig_Eqs <- as_tibble(Sig_Eq)
Sig_Eqs

#1.2
death_country <- Sig_Eqs %>%
  select(COUNTRY,DEATHS) %>%
  group_by(COUNTRY) %>%
  summarise(total_death = sum(DEATHS,na.rm = TRUE)) %>%
  arrange(desc(total_death)) 

death_country  
print(death_country$COUNTRY[1:10]) 

#1.3
Sig_Eqs %>%
  select(YEAR,EQ_PRIMARY,COUNTRY) %>%
  group_by(YEAR) %>%
  summarise(mag_6.0 = sum(EQ_PRIMARY>6.0, na.rm= TRUE)) %>%
  arrange(desc(mag_6.0)) %>%
  ggplot(aes(YEAR,mag_6.0)) +
  geom_line()




#1.4

CountEq_LargestEq <- function(country){
  co_eq = Sig_Eqs %>%
    select(YEAR,COUNTRY,EQ_PRIMARY) %>%
    group_by(COUNTRY) %>%
    summarise(num_eq = sum(EQ_PRIMARY,na.rm= TRUE)) %>%
    spread(COUNTRY,num_eq)
  
  a<- co_eq$country
  
  Sig_Eqs %>%
    select(YEAR,MONTH,DAY,COUNTRY,EQ_PRIMARY) %>%
    group_by(COUNTRY) %>%
    summarise(date = left_join(YEAR,MONTH,DAY))
    
    
    
  
  
    
  
}

co_eq = Sig_Eqs %>%
  select(YEAR,COUNTRY,EQ_PRIMARY) %>%
  group_by(COUNTRY) %>%
  summarise(num_eq = sum(EQ_PRIMARY,na.rm= TRUE)) %>%
  #mutate(coun_eq = ifelse(COUNTRY=='ANTARCTICA', num_eq,NA))
  spread(COUNTRY,num_eq)

co_eq

bb =Sig_Eqs %>%
  
  select(YEAR,MONTH,DAY,COUNTRY,EQ_PRIMARY) %>%
  
  group_by(COUNTRY) %>%
  summarise(max_eq = max(EQ_PRIMARY)) %>%
  spread(COUNTRY,max_eq)
  
  #arrange(desc(max_eq))

print(bb)


  