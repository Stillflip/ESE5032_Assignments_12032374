data3 <- read.csv('PS3.3_data.csv')
data3_tbl <- as_tibble(data3)
# one-way anova
anova_zinc <- aov(pre_nonveg ~ pre_veg,data = data3_tbl)
summary(anova_zinc)
