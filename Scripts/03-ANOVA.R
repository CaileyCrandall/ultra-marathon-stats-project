library(data.table)
library(ggplot2)
library(skimr)
library(corrplot)

ultra <- fread("Data/ultra_rankings_clean.csv")
names(ultra)

model <- aov(pace.min.per.km ~ event.distance.length * athlete.gender, data = ultra[sample(.N,10000)])

summary(model)