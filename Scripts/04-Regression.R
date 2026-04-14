library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)
set.seed(123)

ultra <- fread("Data/ultra_rankings_clean.csv")

df <- ultra

train_index <- sample(1:nrow(df), size = 0.8 * nrow(df))

# split into train and test
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

nrow(train_data)
nrow(test_data)
names(train_data)

unique(train_data$athlete.gender)

unique(train_data[sample(.N, 50000), athlete.gender])
train_model <- lm(pace.min.per.km ~ (athlete.gender + distance.km + event.season + age + event.number.of.finishers + year.of.event)^2,
                  data = train_data)

predictions <- predict(train_model, newdata = test_data,
                       interval = "confidence")

# compare predictions to actual values
results <- data.frame(
  actual = test_data$pace.min.per.km,
  predicted = predictions[, "fit"],
  lower = predictions[, "lwr"],
  upper = predictions[, "upr"]
)

print(results)

# Residuals
results$error <- results$actual - results$predicted

# RMSE
rmse <- sqrt(mean(results$error^2, na.rm = TRUE))

# MAE
mae <- mean(abs(results$error), na.rm = TRUE)

# Test R-squared
sst <- sum((results$actual - mean(results$actual))^2, na.rm = TRUE)
sse <- sum(results$error^2, na.rm = TRUE)
r2_test <- 1 - sse/sst

rmse
mae
r2_test