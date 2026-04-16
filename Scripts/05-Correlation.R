library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)
set.seed(123)


ultra <- fread("Data/ultra_rankings_clean.csv")
## CLAUDE
# ── Country frequency filter (must come before sampling) ──
country_counts <- ultra[, .N, by = event.country]
common_countries <- country_counts[N >= 500, event.country]
ultra <- ultra[event.country %in% common_countries]

# ── Filter to M/F and clean age before sampling ──
ultra <- ultra[
  athlete.gender %in% c("F", "M") &
    !is.na(pace.min.per.km) &
    !is.na(age) &
    !is.na(event.season) &
    !is.na(distance.km)
]
## CLAUDE

df <- ultra[sample(.N, 100000)]


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



#FIXING COUNTRIES

#Significance of countries
#how is country?

# Fit model with just event.country as a main effect (no interactions)
model_country <- lm(pace.min.per.km ~ event.country, data = train_data)
summary(model_country)

# Compare R² with and without
model_no_country <- lm(pace.min.per.km ~ distance.km + I(distance.km^2) + 
                         athlete.gender + age + event.season, 
                       data = train_data)

model_with_country <- lm(pace.min.per.km ~ distance.km + I(distance.km^2) + 
                           athlete.gender + age + event.season + event.country, 
                         data = train_data)

cat("R² without country:", summary(model_no_country)$r.squared, "\n")
cat("R² with country:", summary(model_with_country)$r.squared, "\n")

# ANOVA test — does adding country significantly improve the model?
anova(model_no_country, model_with_country)

# Find countries common to both train and test
common_countries <- intersect(
  unique(train_data$event.country),
  unique(test_data$event.country)
)

# Filter both to only common countries
train_data <- train_data[event.country %in% common_countries]
test_data  <- test_data[event.country %in% common_countries]

# Align factor levels
train_data$event.country <- factor(train_data$event.country)
test_data$event.country  <- factor(test_data$event.country,
                                   levels = levels(train_data$event.country))



# BUILDING THE MODEL
train_model <- lm(pace.min.per.km ~ (year.of.event + event.number.of.finishers + age + event.season + athlete.gender + distance.km + event.country )^2, data = train_data)



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

# ANOVA 
anova(train_model)

