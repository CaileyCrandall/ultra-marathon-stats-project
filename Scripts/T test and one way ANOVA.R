library(data.table)
library(ggplot2)

ultra <- fread("Data/ultra_rankings_clean.csv")
ultra <- ultra[athlete.gender %in% c("F", "M") & !is.na(pace.min.per.km)]

# ============================================================
# 1. Welch's t-test — Gender vs Pace
# ============================================================
cat("\n--- Welch's t-test: Pace by Gender ---\n")
t_result <- t.test(pace.min.per.km ~ athlete.gender, data = ultra)
print(t_result)

# Effect size — Cohen's d
# install.packages("effectsize")
library(effectsize)
cohens_d(pace.min.per.km ~ athlete.gender, data = ultra)

# Visualise
ggplot(ultra, aes(x = athlete.gender, y = pace.min.per.km, fill = athlete.gender)) +
  geom_boxplot(outlier.alpha = 0.02) +
  coord_cartesian(ylim = c(3, 15)) +
  labs(title = "Pace by Gender",
       x = "Gender", y = "Minutes per km")

# ============================================================
# 2. One-way ANOVA — Season vs Pace
# ============================================================
cat("\n--- One-way ANOVA: Pace by Season ---\n")


ultra[, event.season := factor(
  event.season,
  levels = c("Winter", "Spring", "Summer", "Fall")
)]

season_anova <- aov(pace.min.per.km ~ event.season, data = ultra)
summary(season_anova)

# Effect size
eta_squared(season_anova)

# Post-hoc — which seasons differ from each other?
cat("\n--- Tukey HSD Post-hoc Test ---\n")
tukey_result <- TukeyHSD(season_anova)
print(tukey_result)

# Visualise
ggplot(ultra, aes(x = event.season, y = pace.min.per.km, fill = event.season)) +
  geom_boxplot(outlier.alpha = 0.02) +
  coord_cartesian(ylim = c(3, 15)) +
  labs(title = "Pace by Season",
       x = "Season", y = "Minutes per km")