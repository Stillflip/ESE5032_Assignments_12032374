library(dplyr)
data5 <- read.csv('PS3.5_data.csv')
data5_tbl <- as_tibble(data5)

model5 <- lm(Distance ~ Velocity, data = data5_tbl)
coef(model5)

coef(model5)[2]
#5.1
plot(Distance ~ Velocity, data = data5_tbl)

#5.2
abline(model5, lwd = 3, col = "red")   # draw the regression line


