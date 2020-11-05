data7 <- read.csv('data_for_use.csv')
data7_tbl <- as_tibble(data7)

hist(data7_tbl$TEMP)
hist(data7_tbl$RH)
hist(data7_tbl$PM2.5)
hist(data7_tbl$BC)

#7.1 t.test
t.test(data7_tbl$PM2.5,data7_tbl$BC)

#7.2 anova

anova7 <- aov (BC ~ PM2.5,data = data7_tbl)
summary(anova7)

#7.3 lm model
mode7 <- lm(BC ~ RH + TEMP + PM2.5, data = data7_tbl )
summary(mode7)
