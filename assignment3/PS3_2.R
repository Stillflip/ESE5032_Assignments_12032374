library(dplyr)

Bone <- c(rep('Rib 16',times=4),rep('Gastralia',times=6),rep('Dorsal vertebra',times=10),
          rep('Femur',times=4),rep('Tibia',times=5),rep('Metatarsal',times=4),
          rep('Phalange',times=3),rep('Proximal caudal',times=6),rep('Mid-caudal',times=5),
          rep('Distal caudal',times=5))
V2 <- c(11.10,11.22,11.29,11.49,11.32,11.40,11.71,11.60,11.78,12.05,10.61,
                     10.88,11.12,11.24,11.43,10.92,11.20,11.30,11.62,11.70,11.70,11.79,
                     11.91,12.15,11.33,11.41,11.62,12.15,12.30,11.32,11.65,11.96,12.15,
                     11.54,11.89,12.04,10.93,11.01,11.08,11.12,11.28,11.37,11.35,11.43,
                     11.50,11.57,11.92,11.95,12.01,12.25,12.30,12.39)

rex_data <- cbind(Bone,V2)
rex_data <- as_tibble(rex_data)
means_bone<- rex_data %>%
  mutate(Bone_new = factor(Bone,ordered = TRUE))

glimpse(means_bone)

bone1 <- means_bone %>%
  group_by(Bone_new) %>%
  #mutate(meanes =mean(as.numeric(oxygen), na.rm = TRUE) )
  summarise(mean_o=mean(as.numeric(V2)))
  
  
  
  