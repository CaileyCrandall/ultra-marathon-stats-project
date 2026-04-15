# Third model: raw pace with nonlinear predictors

library(data.table)
library(ggplot2)

set.seed(123)

# Load data
ultra <- fread("Data/ultra_rankings_clean.csv")

# Copy dataset
df <- copy(ultra)

# Keep complete usable rows
df <- df[
  !is.na(pace.min.per.km) &
    !is.na(athlete.gender) &
    !is.na(distance.km) &
    !is.na(age) &
    !is.na(event.season) &
    !is.na(event.number.of.finishers) &
    !is.na(year.of.event)
]

# Keep realistic pace values
df <- df[
  pace.min.per.km >= 2 &
    pace.min.per.km <= 60
]

# Keep only male/female rows
df <- df[athlete.gender %in% c("M","F")]

# Convert to factors
df[, athlete.gender := factor(athlete.gender, levels = c("F","M"))]

df[, event.season := factor(
  event.season,
  levels = c("Spring","Summer","Fall","Winter")
)]

# Create transformed variables
df[, log_distance := log(distance.km)]
df[, age2 := age^2]

# Train test split
n <- nrow(df)
train_index <- sample(seq_len(n), size = 0.8 * n)

train_data <- df[train_index]
test_data  <- df[-train_index]

# Fit third model
train_model3 <- lm(
  pace.min.per.km ~ athlete.gender * log_distance +
    age + age2 +
    event.season +
    event.number.of.finishers +
    year.of.event,
  data = train_data
)

# Summary
summary(train_model3)

# Predict on test data
pred3 <- predict(
  train_model3,
  newdata = test_data,
  interval = "prediction"
)

# Put into results table
results3 <- data.frame(
  actual = test_data$pace.min.per.km,
  predicted = pred3[, "fit"],
  lower = pred3[, "lwr"],
  upper = pred3[, "upr"]
)

print(head(results3, 20))
summary(results3)

# Remove missing rows
eval3 <- na.omit(results3)

# Errors
eval3$error <- eval3$actual - eval3$predicted

# Metrics
rmse3 <- sqrt(mean((eval3$actual - eval3$predicted)^2))
mae3  <- mean(abs(eval3$actual - eval3$predicted))

sst3 <- sum((eval3$actual - mean(eval3$actual))^2)
sse3 <- sum((eval3$actual - eval3$predicted)^2)

r2_test3 <- 1 - sse3 / sst3

rmse3
mae3
r2_test3

# Train R^2
train_pred3 <- predict(train_model3)
used_train3 <- model.frame(train_model3)
train_actual3 <- used_train3$pace.min.per.km

train_r2_3 <- 1 -
  sum((train_actual3 - train_pred3)^2) /
  sum((train_actual3 - mean(train_actual3))^2)

train_r2_3

# Predicted vs actual plot
plot(
  eval3$predicted,
  eval3$actual,
  pch = 16,
  cex = .3,
  xlab = "Predicted pace",
  ylab = "Actual pace"
)

abline(0, 1, col = "red")

# Residual plot
plot(
  eval3$predicted,
  eval3$error,
  pch = 16,
  cex = .3,
  xlab = "Predicted pace",
  ylab = "Residual"
)

abline(h = 0, col = "red")