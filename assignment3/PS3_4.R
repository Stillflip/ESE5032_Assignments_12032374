data4 <- read.csv('PS3.4_data.csv')
data4_tbl <- as_tibble(data4)

plot(Temperature ~ Elevation,data= data4)  #plot scatter plot

model <- lm(Temperature ~ Elevation, data = data4) # linear model
coef(model)
abline(model, lwd = 2, col = "red")   # draw theregression line
