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

range(ultra$Year.of.event)       # should show 1798 to 2022
