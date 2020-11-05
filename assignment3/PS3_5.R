data5 <- read.csv('PS3.5_data.csv')
data5_tbl <- as_tibble(data5)

data_5_new <- data5_tbl %>%
  mutate(dis=Distance, vec=abs(Velocity))
model5 <- lm(dis ~ vec, data = data_5_new)
coef(model5)

#5.1
plot(dis ~ vec, data = data_5_new)

#5.2
abline(model5, lwd = 3, col = "red")   # draw the regression line


