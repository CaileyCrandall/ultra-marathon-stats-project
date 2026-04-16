# ANOVA ATTEMPT NUMBER 2
#install.packages("effectsize")

library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)
library(effectsize)   # for eta_squared — install if needed: install.packages("effectsize")

# ── Load data ────────────────────────────────────────────────
ultra <- fread("Data/ultra_rankings_clean.csv")

# ── Clean / filter ───────────────────────────────────────────
# Keep only M/F, non-missing pace
ultra <- ultra[
  athlete.gender %in% c("F", "M") &
    !is.na(pace.min.per.km) &
    !is.na(distance.km) &
    !is.na(distance.group) &
    !is.na(age)
]

# Factor ordering for distance group
ultra[, distance.group := factor(
  distance.group,
  levels = c("~50km", "~100km", "~100mi", "~200km", "200km+")
)]

# Gender: factor with Female as reference
ultra[, athlete.gender := relevel(factor(athlete.gender), ref = "F")]

# Sanity checks
table(ultra$athlete.gender, useNA = "ifany")
table(ultra$distance.group, useNA = "ifany")



# ANOVA — distance.group * athlete.gender

cat("\n--- ANOVA: pace ~ distance.group * athlete.gender ---\n")
model_anova <- aov(pace.min.per.km ~ distance.group * athlete.gender, data = ultra)
summary(model_anova)

# Effect size — more meaningful than p-values at n = 6.5M
# eta^2 close to 0 = small, ~0.06 = medium, ~0.14 = large
cat("\n--- Eta Squared (Effect Size) ---\n")
eta_squared(model_anova)


# ============================================================
# 2. Linear Regression — Model 1: distance.km * gender
# ============================================================
cat("\n--- Regression Model 1: pace ~ distance.km * gender ---\n")
model1 <- lm(pace.min.per.km ~ distance.km * athlete.gender, data = ultra)
summary(model1)
anova(model1)

# Coefficients and crossover point
# (where do male and female predicted paces intersect?)
b0 <- coef(model1)[1]   # intercept (Female baseline)
b1 <- coef(model1)[2]   # slope for distance.km (Female)
b2 <- coef(model1)[3]   # gender offset (Male vs Female at distance = 0)
b3 <- coef(model1)[4]   # interaction: how much Male slope differs from Female

cross <- -b2 / b3
cat("\nCrossover distance (km) where M and F paces are equal:", round(cross, 1), "km\n")

# Plot Model 1
ggplot(ultra[sample(.N, 50000)],
       aes(x = distance.km, y = pace.min.per.km, color = athlete.gender)) +
  geom_point(alpha = 0.08) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Pace vs Distance by Gender (Linear Fit)",
       x = "Distance (km)", y = "Minutes per km",
       color = "Gender")


# ============================================================
# 3. Linear Regression — Model 2: add age
# ============================================================
cat("\n--- Regression Model 2: pace ~ distance.km * gender + age ---\n")
model2 <- lm(pace.min.per.km ~ distance.km * athlete.gender + age, data = ultra)
summary(model2)

# Compare models — lower AIC = better fit
cat("\n--- AIC Comparison ---\n")
cat("Model 1 AIC:", AIC(model1), "\n")
cat("Model 2 AIC:", AIC(model2), "\n")


# ============================================================
# 4. Regression Diagnostics
#    (sample 10k for speed — full data would take very long to plot)
# ============================================================
cat("\n--- Regression Diagnostics (Model 2) ---\n")
diag_sample <- ultra[sample(.N, 10000)]
diag_model  <- lm(pace.min.per.km ~ distance.km * athlete.gender + age, data = diag_sample)

par(mfrow = c(2, 2))
plot(diag_model, main = "Diagnostics: Model 2 (sample n=10,000)")
par(mfrow = c(1, 1))


# ============================================================
# 5. Visualisations
# ============================================================

# -- Interaction plot: median pace by gender x distance group
summary_df <- ultra[,
                    .(median_pace = median(pace.min.per.km, na.rm = TRUE)),
                    by = .(distance.group, athlete.gender)
]

ggplot(summary_df,
       aes(x = distance.group, y = median_pace,
           color = athlete.gender, group = athlete.gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Median Pace by Gender Across Distance Groups",
       x = "Distance Group", y = "Median Minutes per km",
       color = "Gender")

# -- Boxplot: pace by gender x distance group
ggplot(ultra,
       aes(x = distance.group, y = pace.min.per.km, fill = athlete.gender)) +
  geom_boxplot(outlier.alpha = 0.05) +
  coord_cartesian(ylim = c(3, 15)) +
  labs(title = "Pace by Gender Across Distance Groups",
       x = "Distance Group", y = "Minutes per km",
       fill = "Gender")

# Filter first, then sample
plot_data <- ultra[distance.km <= 250][sample(.N, 10000)]

ggplot(plot_data,
       aes(x = distance.km, y = pace.min.per.km, color = athlete.gender)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Pace vs Distance by Gender (Loess, ≤250km)",
       x = "Distance (km)", y = "Minutes per km",
       color = "Gender")
# -- Age vs pace by gender (non-linear loess)
ggplot(ultra[sample(.N, 10000)],
       aes(x = age, y = pace.min.per.km, color = athlete.gender)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Age vs Pace by Gender (Loess)",
       x = "Age", y = "Minutes per km",
       color = "Gender")

# model 3 knowing relationship between age, distance, and pace
ultra[, distance2 := distance.km^2]

model3 <- lm(pace.min.per.km ~ distance.km + distance2 + 
               athlete.gender + age + athlete.gender * distance.km , data = ultra)
summary(model3)