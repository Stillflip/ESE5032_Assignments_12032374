library(MASS)
data(cpus)

# Split into two subsets
sample_index <- sample(nrow(cpus),nrow(cpus)*0.8)
cpu_train <- cpus[sample_index,]
cpu_test  <- cpus[-sample_index,]

#6.1
model6 <- lm(perf ~ syct + mmin + mmax + cach + chmin + chmax, data = cpu_train)
coef(model6)

#predict
perf_pred <- predict(model6,cpu_test)

# Compare predicted values with actual values 
plot(cpu_test$perf, perf_pred)

# Mean predicted value
mean(perf_pred)

# Mean actual value
mean(cpu_test$perf)

# Relative mean bias
(mean(perf_pred) - mean(cpu_test$perf))/mean(cpu_test$perf)*100


