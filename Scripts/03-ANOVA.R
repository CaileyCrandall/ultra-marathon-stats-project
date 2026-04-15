library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)

ultra <- fread("Data/ultra_rankings_clean.csv")
names(ultra)

model <- aov(pace.min.per.km ~ distance.group * athlete.gender, data = ultra[sample(.N,10000)])


summary(model)

#Regression
# Keep only rows needed for this model
df <- ultra[
  !is.na(pace.min.per.km) &
    !is.na(distance.km) &
    !is.na(athlete.gender)
]

# Keep only Female / Male rows
df <- df[athlete.gender %in% c("F", "M")]

# Make gender a factor and set Female as the reference level
df$athlete.gender <- factor(df$athlete.gender)
df$athlete.gender <- relevel(df$athlete.gender, ref = "F")

# Optional: check that coding looks right
table(df$athlete.gender, useNA = "ifany")

# Fit the regression with interaction
model <- lm(pace.min.per.km ~ distance.km * athlete.gender, data = df)

# View results
summary(model)
anova(model)

# PLOT!
ggplot(df[sample(.N,50000)], aes(distance.km, pace.min.per.km, color=athlete.gender)) +
  geom_point(alpha=.08) +
  geom_smooth(method="lm", se=FALSE)


# Coefficients
coef(model)

b0 <- coef(model)[1]
b1 <- coef(model)[2]
b2 <- coef(model)[3]
b3 <- coef(model)[4]

cross <- -b2 / b3
cross