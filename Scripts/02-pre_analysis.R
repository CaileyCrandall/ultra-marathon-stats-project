# Run once to install
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("skimr")
#install.packages("corrplot")

# Run every session to load
library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)

# Make sure it's data.table format
ultra <- fread("Data/ultra_rankings_clean.csv")

# Should print TRUE 
is.data.table(ultra)

# Quick size check
dim(ultra)

# See the first 6 rows
head(ultra)

# All column names
names(ultra)

# summary
skim(ultra)

# number of rows
nrow(ultra)

range(ultra$year.of.event)       # should show 1798 to 2022

table(ultra$race.category)
table(ultra$distance.group)

str(ultra)

# Density histogram for athlete vs age
ggplot(ultra, aes(x=age)) + 
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Athlete Age", 
       x = "Age", y = "Count")
# boxplot for pace by gender (limiting y axis for better interpretation)
ggplot(ultra, aes(x = athlete.gender, y = pace.min.per.km)) + 
  geom_boxplot(fill = "tomato") +
  coord_cartesian(ylim = c(0,15)) +
  labs(title = "Pace by Gender",
       x = "Gender", y = "Minutes per km")

# Box plot for age by distance group
ggplot(ultra, aes(x = distance.group, y = age)) +
  geom_boxplot(fill = "orchid") + 
  labs(title = "Age by Distance Group")
# QQ Plot for normality check
ggplot(ultra[sample(.N,10000)], aes(sample = age)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Age")
# Scatterplot
ggplot(ultra[sample(.N,50000)], aes(x = age, y = pace.min.per.km)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Age vs Pace")

# Violin plot gender vs. pace per km
ultra2 <- ultra[athlete.gender %in% c("F","M")]
ggplot(ultra2, aes(x = athlete.gender, y = pace.min.per.km)) +
  coord_cartesian(ylim = c(0,20)) +
  geom_violin(fill="skyblue") +
  geom_boxplot(width=.1)


summary(ultra$pace.min.per.km)
quantile(ultra$pace.min.per.km, probs = c(.01,.05,.25,.5,.75,.95,.99), na.rm=TRUE)

# histogram for pace
ggplot(ultra, aes(x = pace.min.per.km)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue") +
  coord_cartesian(xlim = c(2,20))

# Scatter plot age vs pace
ggplot(ultra[sample(.N,50000)],
       aes(x = age, y = pace.min.per.km)) +
  geom_point(alpha = 0.12) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Age vs Pace",
       x = "Age",
       y = "Minutes per km")



names(ultra)


# Boxplot of pace by gender within distance group
# Boxplot of pace by gender within distance group
ggplot(
  ultra[athlete.gender %in% c("F","M")],
  aes(
    x = factor(distance.group,
               levels = c("~50km", "~100km", "~100mi", "~200km", "200km+")),
    y = pace.min.per.km,
    fill = athlete.gender
  )
) +
  geom_boxplot(outlier.alpha = 0.05) +
  coord_cartesian(ylim = c(3,20)) +
  labs(title = "Pace by Gender Across Distance Groups",
       x = "Distance Group",
       y = "Minutes per km",
       fill = "Gender")

# Mean/Median pace line plot by distance
summary_df <- ultra[
  athlete.gender %in% c("F","M"),
  .(median_pace = median(pace.min.per.km, na.rm=TRUE)),
  by = .(distance.group, athlete.gender)
]

ggplot(summary_df,
       aes(x = factor(distance.group,
                      levels = c("~50km", "~100km", "~100mi", "~200km", "200km+")),
           y = median_pace,
           color = athlete.gender,
           group = athlete.gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Median Pace by Gender and Distance",
       x = "Distance Group",
       y = "Median Minutes per km",
       color = "Gender")

#Density plot for the 100mi, pace vs gender
ggplot(
  ultra[
    distance.group == "~100mi" &
      athlete.gender %in% c("F","M") &
      !is.na(pace.min.per.km)
  ],
  aes(x = pace.min.per.km, fill = athlete.gender)
) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(3,20))

# To answer the question: Do women relatively improve at larger distances?
summary_df <- ultra[
  athlete.gender %in% c("F","M"),
  .(median_pace = median(pace.min.per.km, na.rm=TRUE)),
  by = .(distance.group, athlete.gender)
]

ultra[, distance.group := factor(
  distance.group,
  levels = c("~50km", "~100km", "~100mi", "~200km", "200km+")
)]

ggplot(summary_df,
       aes(x = distance.group,
           y = median_pace,
           color = athlete.gender,
           group = athlete.gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Median Pace by Gender Across Distance Groups",
       y = "Median Minutes per km",
       x = "Distance Group")

#Have runners gotten faster over time?
year_summary <- ultra[
  !is.na(pace.min.per.km),
  .(
    median_pace = median(pace.min.per.km, na.rm = TRUE),
    n = .N
  ),
  by = year.of.event
][order(year.of.event)]

ggplot(year_summary,
       aes(x = year.of.event, y = median_pace)) +
  geom_line(color= "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Median Pace by Year",
       x = "Year of Event",
       y = "Median Minutes per km")

#filtered version of above plot
ggplot(year_summary[year.of.event >= 1950 & year.of.event <= 2022, ],
       aes(x = year.of.event, y = median_pace)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Median Pace by Year (1950–2022)",
       x = "Year of Event",
       y = "Median Minutes per km")

# bar chart for event season (remove NA values)

ggplot(ultra[!is.na(event.season), ], aes(x = event.season)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Events by Season",
       x = "Season",
       y = "Count")

ultra[, event.season := factor(
  event.season,
  levels = c("Winter","Spring","Summer","Fall")
)]

ggplot(ultra, aes(x = event.season, y = pace.min.per.km)) +
  geom_boxplot(fill = "orange") +
  coord_cartesian(ylim = c(3,20))

# Country top performers
top_ctry <- ultra[, .N, by=event.country][order(-N)][1:10]
top_ctry

top5 <- c("USA","FRA","RSA","JPN","ITA")

country_summary <- ultra[
  event.country %in% top5,
  .(
    median_pace = median(pace.min.per.km, na.rm = TRUE),
    n = .N
  ),
  by = event.country
][order(median_pace)]

ggplot(country_summary,
       aes(x = reorder(event.country, median_pace),
           y = median_pace)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Median Pace by Top 5 Countries",
       x = "Country",
       y = "Median Minutes per km")

ggplot(country_summary,
       aes(x = event.country, y = median_pace)) +
  geom_point(size = 4) +
  geom_segment(aes(x = event.country,
                   xend = event.country,
                   y = 0,
                   yend = median_pace))

#correlation matrix for numeric variables
num_vars <- ultra[, .(pace.min.per.km, age, distance.km,
                      event.number.of.finishers, year.of.event,
                      performance.seconds)]
cor_matrix <- cor(num_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper")

