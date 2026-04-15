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

table(train_data$athlete.gender, useNA="ifany")
table(test_data$athlete.gender, useNA = "ifany")
levels(train_data$athlete.gender)
levels(test_data$athlete.gender)

#FIXING GENDER
train_data <- train_data[athlete.gender %in% c("F","M")]
test_data  <- test_data[athlete.gender %in% c("F","M")]

train_data$athlete.gender <- factor(train_data$athlete.gender)
test_data$athlete.gender  <- factor(test_data$athlete.gender,
                                    levels = levels(train_data$athlete.gender))

train_data$athlete.gender <- relevel(train_data$athlete.gender, ref = "F")
test_data$athlete.gender  <- factor(test_data$athlete.gender,
                                    levels = c("F","M"))



unique(train_data[sample(.N, 50000), athlete.gender])



# BUILDING THE MODEL
train_model <- lm(pace.min.per.km ~ (athlete.gender + distance.km + event.season + age + event.number.of.finishers + year.of.event)^2,
                  data = train_data)

predictions <- predict(train_model, newdata = test_data,
                       interval = "prediction")

# compare predictions to actual values
results <- data.frame(
  actual = test_data$pace.min.per.km,
  predicted = predictions[, "fit"],
  lower = predictions[, "lwr"],
  upper = predictions[, "upr"]
)

print(results)
summary(results)
summary(train_model)

plot(results$predicted, results$actual,
     pch=16, cex=.3)
abline(0,1,col="red")

# Residuals
results$error <- results$actual - results$predicted


# Test R-squared
eval <- na.omit(results)

# RMSE
rmse <- sqrt(mean((eval$actual - eval$predicted)^2))

# MAE
mae  <- mean(abs(eval$actual - eval$predicted))

sst <- sum((eval$actual - mean(eval$actual))^2)
sse <- sum((eval$actual - eval$predicted)^2)

#R^2
r2_test <- 1 - sse/sst


rmse
mae
r2_test

#Computing train r^2 manually (checking for reason)
train_pred <- predict(train_model)

used_train <- model.frame(train_model)

train_actual <- used_train$pace.min.per.km

train_r2_manual <- 1 - sum((train_actual - train_pred)^2) /
  sum((train_actual - mean(train_actual))^2)

train_r2_manual

var(train_actual)
var(results$actual, na.rm=TRUE)
sd(results$error, na.rm=TRUE)
summary(train_data$distance.km)
summary(test_data$distance.km)


#Residual Plot
plot(eval$predicted, eval$actual - eval$predicted)
abline(h=0, col="red")
