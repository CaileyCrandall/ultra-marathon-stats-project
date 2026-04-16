# FINAL REGRESSION MODEL 

# ============================================================
# Ultra Marathon Pace Analysis — Final Clean Model
# ============================================================
# Research Question: What factors predict pace (min/km) in
# ultra marathon running?
#
# Predictors:
#   distance.km     — race distance (continuous, with quadratic term)
#   athlete.gender  — M/F (justified by t-test, Cohen's d)
#   age             — athlete age at race (justified by ANOVA)
#   event.season    — season of race (justified by Tukey HSD)
#   event.country   — country of race (justified by R² jump)
# ============================================================

library(data.table)
library(ggplot2)
library(effectsize)

set.seed(123)

# ── 1. Load data ─────────────────────────────────────────────
ultra <- fread("Data/ultra_rankings_clean.csv")

# ── 2. Filter and clean ──────────────────────────────────────

# Keep only frequent countries (avoids unseen level errors in prediction)
country_counts  <- ultra[, .N, by = event.country]
common_countries <- country_counts[N >= 500, event.country]
ultra <- ultra[event.country %in% common_countries]

# Keep only M/F drop missing values in key variables
ultra <- ultra[
  athlete.gender %in% c("F", "M") &
    !is.na(pace.min.per.km) &
    !is.na(age) &
    !is.na(event.season) &
    !is.na(distance.km) &
    !is.na(event.country)
  ]

ultra <- ultra[sample(.N, 100000)]
# Factor levels
ultra[, athlete.gender := relevel(factor(athlete.gender), ref = "F")]
ultra[, event.season   := factor(event.season,
                                 levels = c("Winter","Spring","Summer","Fall"))]
ultra[, event.country  := factor(event.country)]

# Quadratic distance term
ultra[, distance2 := distance.km^2]

cat("Rows after cleaning:", nrow(ultra), "\n")

# ── 3. Train / Test Split ────────────────────────────────────
df          <- ultra[sample(.N, 100000)]
train_index <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data  <- df[train_index, ]
test_data   <- df[-train_index, ]

# Align factor levels between train and test
test_data[, athlete.gender := factor(athlete.gender,
                                     levels = levels(train_data$athlete.gender))]
test_data[, event.season   := factor(event.season,
                                     levels = levels(train_data$event.season))]
test_data[, event.country  := factor(event.country,
                                     levels = levels(train_data$event.country))]

cat("Train rows:", nrow(train_data), "\n")
cat("Test rows: ", nrow(test_data),  "\n")

# ── 4. Fit Final Model ───────────────────────────────────────
# Every predictor is justified by prior statistical tests:
#   distance.km + distance2  → non-linear relationship shown by loess
#   athlete.gender           → Welch t-test p < 2e-16, Cohen's d = 0.20
#   age                      → significant in prior regression models
#   event.season             → one-way ANOVA p < 2e-16, eta² = 0.05
#   event.country            → ANOVA R² jump from 0.099 → 0.316
names(train_data)
final_model <- lm(pace.min.per.km ~ year.of.event + distance.km + distance2 +
                    athlete.gender + age +
                    event.season + event.country,
                  data = train_data)

cat("\n--- Final Model Summary ---\n")
summary(final_model)

# ── 5. Predictions on Test Set ───────────────────────────────
predictions <- predict(final_model, newdata = test_data,
                       interval = "prediction")

results <- data.frame(
  actual    = test_data$pace.min.per.km,
  predicted = predictions[, "fit"],
  lower     = predictions[, "lwr"],
  upper     = predictions[, "upr"]
)

eval <- na.omit(results)
eval$error <- eval$actual - eval$predicted

# ── 6. Model Evaluation Metrics ─────────────────────────────
train_r2  <- summary(final_model)$r.squared
train_adj <- summary(final_model)$adj.r.squared

sst      <- sum((eval$actual - mean(eval$actual))^2)
sse      <- sum((eval$actual - eval$predicted)^2)
r2_test  <- 1 - sse / sst
rmse     <- sqrt(mean(eval$error^2))
mae      <- mean(abs(eval$error))

cat("\n--- Model Performance ---\n")
cat("Train R²:         ", round(train_r2,  4), "\n")
cat("Train Adjusted R²:", round(train_adj, 4), "\n")
cat("Test R²:          ", round(r2_test,   4), "\n")
cat("RMSE:             ", round(rmse,       4), "min/km\n")
cat("MAE:              ", round(mae,        4), "min/km\n")

# Overfitting check
cat("\nDifference (Train - Test R²):", round(train_r2 - r2_test, 4),
    "— ideally < 0.02\n")

# ── 7. Plots ─────────────────────────────────────────────────

# -- Actual vs Predicted
ggplot(eval[sample(nrow(eval), 5000), ],
       aes(x = predicted, y = actual)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(title = "Actual vs Predicted Pace",
       x = "Predicted (min/km)", y = "Actual (min/km)")

# -- Residual Plot
ggplot(eval[sample(nrow(eval), 5000), ],
       aes(x = predicted, y = error)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1) +
  labs(title = "Residuals vs Fitted",
       x = "Predicted (min/km)", y = "Residual (min/km)")

# -- Pace by Gender
ggplot(train_data,
       aes(x = athlete.gender, y = pace.min.per.km, fill = athlete.gender)) +
  geom_boxplot(outlier.alpha = 0.02) +
  coord_cartesian(ylim = c(3, 15)) +
  labs(title = "Pace by Gender",
       x = "Gender", y = "Minutes per km")

# -- Pace by Season
ggplot(train_data,
       aes(x = event.season, y = pace.min.per.km, fill = event.season)) +
  geom_boxplot(outlier.alpha = 0.02) +
  coord_cartesian(ylim = c(3, 15)) +
  labs(title = "Pace by Season",
       x = "Season", y = "Minutes per km")

# -- Age vs Pace by Gender (loess)
ggplot(train_data[sample(.N, 20000)],
       aes(x = age, y = pace.min.per.km, color = athlete.gender)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Age vs Pace by Gender",
       x = "Age", y = "Minutes per km",
       color = "Gender")

#Distance vs pace by gender
ggplot(train_data[distance.km <= 250][sample(.N, 20000)],
       aes(x = distance.km, y = pace.min.per.km, color = athlete.gender)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE) +
  coord_cartesian(ylim = c(6, 14)) +
  labs(title = "Distance vs Pace by Gender (Quadratic Fit, ≤250km)",
       x = "Distance (km)", y = "Minutes per km",
       color = "Gender")

# Extract coefficients for model equation
coef(final_model)