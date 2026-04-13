# LOAD LIBRARIES
# install.packages("data.table")
library(data.table)
library(data.table)

df <- fread("C:/Users/Cailey/OneDrive - University of South Florida/USF/Spring 26/Stats 2/Project/Ultra_Marathon_Stats_Project/Data/TWO_CENTURIES_OF_UM_RACES_full.csv")
# Load data
# Standardize column names (lowercase + replace spaces and slashes with dots)
names(df) <- tolower(names(df))
names(df) <- gsub(" ", ".", names(df))
names(df) <- gsub("/", ".", names(df))
names(df)
# see the columns and the first few rows
head(df)
names(df)

# this counts how many races each unique athlete ID participated in
id_counts <- table(df$Athlete.ID)

# This shows the "Top 10" most frequent runners

sort(id_counts, decreasing = TRUE) |> head(10)

# CONVERTING VARIABLES



# DISTANCES

#check what values exists
unique(df$event.distance.length)

# make a copy so you do not overwrite your original working df
df_clean <- copy(df)

# original raw distance column
x <- df_clean$Event.distance.length

# number of stages (defaults to 1 if no stage info is present)
df_clean[, stages := fifelse(
  grepl("Etappen", x),
  as.integer(sub(".*\\/([0-9]+)Etappen$", "\\1", x)),
  1L
)]

# remove the "/...Etappen" part so we can parse the main event value
df_clean[, distance_main := sub("\\/.*$", "", x)]

# numeric part
df_clean[, event_value := as.numeric(sub("^([0-9]+\\.?[0-9]*).*", "\\1", distance_main))]

# unit
df_clean[, event_unit := sub("^[0-9]+\\.?[0-9]*", "", distance_main)]

# race category
df_clean[, race_category := fifelse(
  event_unit %in% c("km", "mi") & stages == 1L, "fixed_distance",
  fifelse(event_unit == "h" & stages == 1L, "timed_event",
          fifelse(stages > 1L, "stage_race", "other"))
)]

# convert miles to km for fixed-distance races
df_clean[, distance_km := fifelse(
  event_unit == "km", event_value,
  fifelse(event_unit == "mi", event_value * 1.60934, NA_real_)
)]

# timed events in hours
df_clean[, event_hours := fifelse(event_unit == "h", event_value, NA_real_)]

#sanity checking
names(df_clean)
df_clean[1:15, .(
  event.distance.length,
  event_value,
  event_unit,
  stages,
  race_category,
  distance_km,
  event_hours
)]
# more checking
df_clean[event_unit == "mi",
         .(event.distance.length, event_value, distance_km)][1:10]
# making sure the race categories look reasonable
table(df_clean$race_category)
# There are 18,840 rows where the distance format doesn't match the expected patterns
unique(df_clean[race_category == "other", event.distance.length])[1:30]
# fixing capitalization differences
df_clean[, event.distance.length :=
           gsub("Km", "km", event.distance.length)]

df_clean[, event.distance.length :=
           gsub("k$", "km", event.distance.length)]
# fixing mile spellings
df_clean[, event.distance.length :=
           gsub("mile", "mi", event.distance.length)]
# convert day races to hours
df_clean[grepl("d$", event.distance.length),
         event.distance.length :=
           paste0(as.numeric(sub("d", "", event.distance.length)) * 24, "h")]
# remove obvious junk
df_clean <- df_clean[event.distance.length != "None"]
# repeat the parsing section
table(df_clean$race_category)
# there are still 17,787 "other" categories. Let's just analyze fixed-distance races.
df_analysis <- df_clean[race_category == "fixed_distance"]
table(df_analysis$race_category)

# CLEANING ATHLETE PERFORMANCE

# Looking at unique formats
unique(df_analysis$athlete.performance)[1:50]

# check if km still appears in performance when filtering to only fixed distance races
df_analysis[grepl("km", athlete.performance), unique(athlete.performance)][1:20]

# how many missing performance times?
sum(is.na(df_analysis$athlete.performance))

# remove trailing " h" and create cleaned version
df_analysis[, perf.clean := sub(" h$", "", athlete.performance)]

# detect rows that include days, like "2d 15:05:00"
df_analysis[, has.days := !is.na(perf.clean) & grepl("d", perf.clean)]

# count unusual formats
df_analysis[
  !is.na(perf.clean) &
    !grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean) &
    !grepl("^[0-9]+d [0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean),
  .N
]

# inspect unusual formats
unique(
  df_analysis[
    !is.na(perf.clean) &
      !grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean) &
      !grepl("^[0-9]+d [0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean),
    perf.clean
  ]
)[1:50]

# initialize output column
df_analysis[, performance.seconds := NA_real_]

# -----------------------------
# rows WITHOUT days: hh:mm:ss
# -----------------------------
valid.no.days <- df_analysis[
  !is.na(perf.clean) &
    has.days == FALSE &
    grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean)
]

time.parts <- tstrsplit(valid.no.days$perf.clean, ":")

df_analysis[
  !is.na(perf.clean) &
    has.days == FALSE &
    grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", perf.clean),
  performance.seconds :=
    as.numeric(time.parts[[1]]) * 3600 +
    as.numeric(time.parts[[2]]) * 60 +
    as.numeric(time.parts[[3]])
]

# -----------------------------
# rows WITH days: Xd hh:mm:ss
# -----------------------------
df_analysis[
  !is.na(perf.clean) & has.days == TRUE,
  days := as.numeric(sub("d.*", "", perf.clean))
]

df_analysis[
  !is.na(perf.clean) & has.days == TRUE,
  time.only := sub(".*d ", "", perf.clean)
]

valid.days <- df_analysis[
  !is.na(time.only) &
    grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", time.only)
]

day.time <- tstrsplit(valid.days$time.only, ":")

df_analysis[
  !is.na(time.only) &
    grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", time.only),
  performance.seconds :=
    days * 86400 +
    as.numeric(day.time[[1]]) * 3600 +
    as.numeric(day.time[[2]]) * 60 +
    as.numeric(day.time[[3]])
]

# quick check
summary(df_analysis$performance.seconds)

# inspect rows still not converted
df_analysis[
  is.na(performance.seconds) & !is.na(athlete.performance),
  .(athlete.performance, perf.clean)
][1:50]

# sanity check for performance
#checking column type
str(df_analysis$performance.seconds)
# checking for missing values
sum(is.na(df_analysis$performance.seconds))
# checking summary statistics
summary(df_analysis$performance.seconds)
# checking largest times (for multi day races)
df_analysis[order(-performance.seconds)][1:10,
                                         .(athlete.performance, performance.seconds)]
# checking smallest times
df_analysis[order(performance.seconds)][1:10,
                                        .(athlete.performance, performance.seconds, distance_km)]
# inspecting 2 missing values
df_analysis[is.na(performance.seconds)]
# removing the missing values
df_analysis <- df_analysis[!is.na(performance.seconds)]
# confirming that missing values are gone
sum(is.na(df_analysis$performance.seconds))

#removing "0" times
df_analysis <- df_analysis[performance.seconds > 0]

# checking races are over 500km(extremely long)
df_analysis[distance_km > 500, .N]
df_analysis[distance_km > 500,
            .(event.name, event.distance.length, distance_km)][1:20]
# Since only 0.045% of the data set has distances larger than 500km, we will remove them
df_analysis <- df_analysis[distance_km <= 500]
# confirming data summary
summary(df_analysis$distance_km)

# checking for impossible paces, now that time and distance are both cleaned
df_analysis[, pace_min_per_km := (performance.seconds / 60) / distance_km]
summary(df_analysis$pace_min_per_km)

# dealing with very fast paces
df_analysis[pace_min_per_km < 2,
            .(event.name, distance_km, athlete.performance,
              performance.seconds, pace_min_per_km)][1:20]
# revealing only rows with impossible paces
df_analysis[!is.na(pace_min_per_km) & pace_min_per_km < 2]
df_analysis[pace_min_per_km < 2, .N]
# there is only 1 row like this. We will remove it
df_analysis <- df_analysis[pace_min_per_km >= 2]

# dealing with very slow paces
df_analysis[pace_min_per_km > 60,
            .(event.name, distance_km, athlete.performance,
              performance.seconds, pace_min_per_km)][1:20]
df_analysis[pace_min_per_km > 60, .N]
# removing them. there are only 21 rows like this
df_analysis <- df_analysis[
  pace_min_per_km >= 2 & pace_min_per_km <= 60
]

# checking summary of paces
summary(df_analysis$pace_min_per_km)

# CHECKING CLEANED DATA SO FAR
names(df_analysis)
# going to remove the helper columns
df_analysis[, c("perf_clean","has_days","days","time_only",
                "distance_main","event_value","event_unit") := NULL]
# CLEANING EVENT DATES
names(df_analysis)
df_analysis[sample(.N, 50), event.dates]

# If the date entry contains a range, we will choose the starting date
df_analysis[, event.dates := gsub("^([0-9]{2})\\.-[0-9]{2}\\.", "\\1.", event.dates)]
# Fixing the formatting
df_analysis[, event.dates := as.Date(event.dates, format = "%d.%m.%Y")]

# checking the results
class(df_analysis$event.dates)
sum(is.na(df_analysis$event.dates))
df_analysis[sample(.N, 20), event.dates]
#checking how many are NA
sum(is.na(df_analysis$event.dates))
df_analysis[is.na(event.dates), .(event.dates, event.name)][1:20]
# there are 47230 NA dates

# AGE VARIABLE
summary(df_analysis$Athlete.year.of.birth)
sum(is.na(df_analysis$Athlete.year.of.birth))
# there are 541139 NA years of birth

# create age variable
df_analysis[,Age := Year.of.event - Athlete.year.of.birth]

summary(df_analysis$Age)
# inspecting super young athletes
df_analysis[Age < 15,
            .(Year.of.event, event.dates, event.name, Athlete.year.of.birth, Athlete.gender, Age)][1:30]

# inspecting super old athletes
df_analysis[Age > 100,
            .(Year.of.event, event.dates, event.name, Athlete.year.of.birth, Athlete.gender, Age)][1:30]
# seems to be a pattern of using birth year 1900/1901 when birth year is unknown
# filtering extreme ages due to data errors
df_analysis <- df_analysis[Age >= 10 & Age <= 105]
range(df_analysis$Age, na.rm = TRUE)

# PACE
class(df_analysis$pace_min_per_km)
summary(df_analysis$pace_min_per_km)

# checking how pace changes with distance
df_analysis[, .N, by = event.distance.length]

# compute average pace by distance
distance_pace <- df_analysis[
  !is.na(pace_min_per_km),
  .(mean_pace = mean(pace_min_per_km),
    median_pace = median(pace_min_per_km),
    n = .N),
  by = event.distance.length
]

distance_pace

# sort in nicely
distance_pace[order(mean_pace)]

# FIRST PLOT! Quick visualization for pace vs distance
sample_data <- df_analysis[sample(.N, 200000)]

plot(sample_data$distance_km,
     sample_data$pace_min_per_km,
     pch = 16,
     cex = 0.3,
     xlab = "Distance (km)",
     ylab = "Pace (min/km)")

# CLEANING DISTANCE VARIABLE CATEGORIES
df_analysis[, distance_group := cut(
  distance_km,
  breaks = c(0, 60, 110, 170, 250, Inf),
  labels = c("~50km", "~100km", "~100mi", "~200km", "200km+")
)]
# checking distribution of distance group
df_analysis[, .N, by = distance_group]

# CLEANING GENDER
unique(df_analysis$Athlete.gender)
# counting NA genders- there are none
sum(is.na(df_analysis$Athlete.gender))
df_analysis[Athlete.gender == "", Athlete.gender := NA]
df_analysis[Athlete.gender == "X", Athlete.gender := NA]

# CLEANING COUNTRY CODES
unique(df_analysis$Athlete.country)

#making everything uppercase
df_analysis[,Athlete.country := toupper(trimws(Athlete.country))]
unique(df_analysis$Athlete.country)
# checking for blanks or missing-style entries
df_analysis[Athlete.country == "", .N]
sum(is.na(df_analysis$Athlete.country))

#checking country frequencies
country_counts <- df_analysis[, .N, by = Athlete.country][order(Athlete.country)]
country_counts

#checking performance times
summary(df_analysis$performance.seconds)

#checking duplicate race entries
sum(duplicated(df_analysis))
df_analysis[, .N, by = .(Athlete.ID, event.name, event.dates)][N > 1]
# duplicates exist. Let's inspect one of them
df_analysis[
  Athlete.ID == 1652 &
    event.name == "Balaton Szupermaraton Stage 2 Fonyód - Szigllget (HUN)"
]

# Looks like we have identical athlete ID's matching to different people. Let's inspect
df_analysis[, unique_birth_years := uniqueN(Athlete.year.of.birth), by = Athlete.ID]
df_analysis[unique_birth_years > 1]

# counting how many athlete ID's have > 1 birth years correlated to them
df_analysis[unique_birth_years > 1, uniqueN(Athlete.ID)]

# counting how many unique athlete ID's there are
uniqueN(df_analysis$Athlete.ID)

# how many rows are associated with these faulty athlete ID's?
bad_ids <- df_analysis[unique_birth_years > 1, unique(Athlete.ID)]

df_analysis[Athlete.ID %in% bad_ids, .N]

# 11-12% of the dataset involves ID's that map to multiple birth years

sum(duplicated(df_analysis))
# removing exact duplicate rows
df_analysis <- unique(df_analysis)
# checking how many races most athletes run. Trying to see how independent our dataset is
df_analysis[, .N, by = Athlete.ID][, summary(N)]

#found that many different athletes were assigned the anonymized ID
df_analysis[, .N, by = Athlete.ID][order(-N)][1]
# it is athlete ID 236
df_analysis[Athlete.ID == 236]
# checking how many birth years athlete ID 236 has
df_analysis[Athlete.ID == 236, unique(Athlete.year.of.birth)]
# they are all associated with the birth year 1967
# how many athletes have a birth year of 1967?
df_analysis[Athlete.year.of.birth == 1967, .N]
# taking a further look at ID 236
df_analysis[Athlete.ID == 236,
            .(Year.of.event, event.name, Athlete.year.of.birth)][1:30]

# EXTRACT EVENT COUNTRY FROM EVENT NAME
df_analysis[, Event.country := sub(".*\\(([^)]+)\\)$", "\\1", event.name)]

# check results
unique(df_analysis$Event.country)[1:20]

# check frequency
df_analysis[, .N, by = Event.country][order(-N)][1:20]

# how many missing?
sum(is.na(df_analysis$Event.country))

#how many unique countries?
uniqueN(df_analysis$Event.country)

# EVENT NUMBER OF FINISHERS
# checking variable type
class(df_analysis$Event.number.of.finishers)
sum(is.na(df_analysis$Event.number.of.finishers))
summary(df_analysis$Event.number.of.finishers)
# checking for events with zero finishes?
df_analysis[Event.number.of.finishers == 0, .N]
# there are only 10. Removing them
df_analysis <- df_analysis[Event.number.of.finishers > 0]

# ATHLETE CLUB
# how many are missing a club?
sum(is.na(df_analysis$Athlete.club))
df_analysis[Athlete.club == "", .N]
uniqueN(df_analysis$Athlete.club)

# ATHLETE AGE CATEGORY
unique(df_analysis$Athlete.age.category)
df_analysis[, .N, by = Athlete.age.category][order(Athlete.age.category)]
# converting 144 blank entries to NA
df_analysis[Athlete.age.category == "", Athlete.age.category := NA]
# checking consistency with gender
df_analysis[
  grepl("^M", Athlete.age.category) & Athlete.gender == "F"
]
df_analysis[
  grepl("^W", Athlete.age.category) & Athlete.gender == "M"
]
# There are 4 rows where athlete gender is M and age category is W45 or W50
# correcting the prefix
df_analysis[
  grepl("^W", Athlete.age.category) & Athlete.gender == "M",
  Athlete.age.category := sub("^W", "M", Athlete.age.category)
]
# verifying the fix
df_analysis[
  grepl("^W", Athlete.age.category) & Athlete.gender == "M"
]

# EVENT SEASON
#extracting the month
df_analysis[, event_month := as.integer(format(event.dates, "%m"))]
# creating the season variables
df_analysis[, event_season :=
              fifelse(event_month %in% c(12,1,2), "Winter",
                      fifelse(event_month %in% c(3,4,5), "Spring",
                              fifelse(event_month %in% c(6,7,8), "Summer",
                                      fifelse(event_month %in% c(9,10,11), "Fall", NA_character_))))]
# checking the distribution
df_analysis[, .N, by = event_season]
# there are <NA> event seasons. Investigating
sum(is.na(df_analysis$event_season))
sum(is.na(df_analysis$event.dates))
# exactly 43377 <NA>s for both categories

# last sanity check
# creating a numeric dataset
numeric_vars <- df_analysis[, .(
  distance_km,
  performance.seconds,
  pace_min_per_km,
  Age,
  Event.number.of.finishers
)]

# computing a correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix

# Saving clean data as an R object
saveRDS(df_analysis, "Data/ultra_rankings_clean.rds")
# also saving as a CSV file
fwrite(df_analysis, "Data/ultra_rankings_clean.csv")

getwd()