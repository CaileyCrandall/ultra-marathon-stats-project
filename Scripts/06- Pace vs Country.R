library(data.table)
library(ggplot2)

nrows()
# ==========================================
# EXPLORE RELATIONSHIP:
# pace.min.per.km vs event.country
# ==========================================

# Make copy
country_df <- copy(df)

# Keep needed rows
country_df <- country_df[
  !is.na(pace.min.per.km) &
    !is.na(event.country) &
    pace.min.per.km >= 2 &
    pace.min.per.km <= 60
]

# Convert country to factor
country_df$event.country <- factor(country_df$event.country)

# ==========================================
# 1. Summary of countries
# ==========================================
cat("Number of countries:\n")
print(length(unique(country_df$event.country)))

cat("\nTop 20 countries by observations:\n")
print(sort(table(country_df$event.country), decreasing = TRUE)[1:20])

# ==========================================
# 2. Mean / Median pace by country
# ==========================================
country_stats <- country_df[
  ,
  .(
    N = .N,
    mean_pace = mean(pace.min.per.km),
    median_pace = median(pace.min.per.km),
    sd_pace = sd(pace.min.per.km)
  ),
  by = event.country
]

# Keep countries with enough data
country_stats <- country_stats[N >= 1000]

# Fastest countries by mean pace
cat("\nTop 15 Fastest Countries (lowest mean pace):\n")
print(country_stats[order(mean_pace)][1:15])

# Slowest countries by mean pace
cat("\nTop 15 Slowest Countries (highest mean pace):\n")
print(country_stats[order(-mean_pace)][1:15])

# ==========================================
# 3. Boxplot of Top 15 Countries by sample size
# ==========================================
top15 <- names(sort(table(country_df$event.country),
                    decreasing = TRUE))[1:15]

plot_df <- country_df[event.country %in% top15]

ggplot(plot_df,
       aes(x = reorder(event.country, pace.min.per.km, median),
           y = pace.min.per.km)) +
  geom_boxplot(outlier.alpha = 0.15) +
  coord_flip() +
  labs(
    title = "Pace Distribution for Top 15 Countries",
    x = "Country",
    y = "Pace (min/km)"
  )

# ==========================================
# 4. ANOVA test: does pace differ by country?
# ==========================================
country_model <- aov(pace.min.per.km ~ event.country,
                     data = country_df)

cat("\nANOVA Results:\n")
print(summary(country_model))

# ==========================================
# 5. Effect size (eta squared)
# ==========================================
ss_total <- sum((country_df$pace.min.per.km -
                   mean(country_df$pace.min.per.km))^2)

ss_country <- sum(
  tapply(country_df$pace.min.per.km,
         country_df$event.country,
         function(x) length(x) *
           (mean(x) - mean(country_df$pace.min.per.km))^2)
)

eta_sq <- ss_country / ss_total

cat("\nEta Squared for Country Effect:\n")
print(eta_sq)

# ==========================================
# 6. Mean pace plot
# ==========================================
ggplot(country_stats[order(mean_pace)][1:20],
       aes(x = reorder(event.country, -mean_pace),
           y = mean_pace)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Countries by Average Pace",
    x = "Country",
    y = "Mean Pace (min/km)"
  )


