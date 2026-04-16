# =========================================
# ULTRA MARATHON DATA CLEANING SCRIPT
# =========================================

# ---------- LOAD LIBRARIES ----------
library(data.table)

# ---------- LOAD DATA ----------
df <- fread(
  "C:/Users/Cailey/OneDrive - University of South Florida/USF/Spring 26/Stats 2/Project/Ultra_Marathon_Stats_Project/Data/TWO_CENTURIES_OF_UM_RACES_full.csv"
)

# ---------- STANDARDIZE COLUMN NAMES ----------
names(df) <- tolower(names(df))
names(df) <- gsub(" ", ".", names(df))
names(df) <- gsub("/", ".", names(df))
names(df) <- gsub("\\.+", ".", names(df))


# Quick check
names(df)
head(df)

# ---------- OPTIONAL: BASIC ID CHECK ----------
id_counts <- table(df$athlete.id)
sort(id_counts, decreasing = TRUE)[1:10]

# =========================================
# DISTANCE CLEANING
# =========================================

df_clean <- copy(df)

# clean and standardize raw distance strings
df_clean[, event.distance.length := trimws(event.distance.length)]
df_clean[, event.distance.length := gsub("Km", "km", event.distance.length, fixed = TRUE)]
df_clean[, event.distance.length := gsub("K$", "km", event.distance.length)]
df_clean[, event.distance.length := gsub("k$", "km", event.distance.length)]
df_clean[, event.distance.length := gsub("miles?", "mi", event.distance.length, ignore.case = TRUE)]

# convert day races like "6d" to hours
df_clean[grepl("^[0-9]+d$", event.distance.length),
         event.distance.length :=
           paste0(as.numeric(sub("d", "", event.distance.length)) * 24, "h")]

# remove obvious junk
df_clean <- df_clean[!is.na(event.distance.length)]
df_clean <- df_clean[event.distance.length != ""]
df_clean <- df_clean[event.distance.length != "None"]

# original cleaned distance column
x <- df_clean$event.distance.length

# number of stages
df_clean[, stages := fifelse(
  grepl("Etappen", x),
  as.integer(sub(".*\\/([0-9]+)Etappen$", "\\1", x)),
  1L
)]

# remove trailing stage info for parsing
df_clean[, distance.main := sub("\\/.*$", "", x)]

# numeric part
df_clean[, event.value := as.numeric(sub("^([0-9]+\\.?[0-9]*).*", "\\1", distance.main))]

# unit
df_clean[, event.unit := sub("^[0-9]+\\.?[0-9]*", "", distance.main)]

# race category
df_clean[, race.category := fifelse(
  event.unit %in% c("km", "mi") & stages == 1L, "fixed_distance",
  fifelse(event.unit == "h" & stages == 1L, "timed_event",
          fifelse(stages > 1L, "stage_race", "other"))
)]

# convert miles to km
df_clean[, distance.km := fifelse(
  event.unit == "km", event.value,
  fifelse(event.unit == "mi", event.value * 1.60934, NA_real_)
)]

# timed events in hours
df_clean[, event.hours := fifelse(event.unit == "h", event.value, NA_real_)]

# checks
table(df_clean$race.category)
head(unique(df_clean[race.category == "other", event.distance.length]), 30)

# keep only fixed-distance races for analysis
df_analysis <- copy(df_clean[race.category == "fixed_distance"])



# =========================================
# ATHLETE PERFORMANCE CLEANING
# =========================================

# helper cleaned performance string
df_analysis[, perf.clean := sub(" h$", "", athlete.performance)]

# indicator for multi-day format like "2d 15:05:00"
df_analysis[, has.days := !is.na(perf.clean) & grepl("d", perf.clean)]

# initialize output
df_analysis[, performance.seconds := NA_real_]

# rows WITHOUT days: hh:mm:ss
no_days_idx <- which(
  !is.na(df_analysis$perf.clean) &
    grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", df_analysis$perf.clean)
)

if (length(no_days_idx) > 0) {
  time.parts <- tstrsplit(df_analysis$perf.clean[no_days_idx], ":")
  df_analysis[no_days_idx,
              performance.seconds :=
                as.numeric(time.parts[[1]]) * 3600 +
                as.numeric(time.parts[[2]]) * 60 +
                as.numeric(time.parts[[3]])]
}

# rows WITH days: Xd hh:mm:ss
days_idx <- which(
  !is.na(df_analysis$perf.clean) &
    grepl("^[0-9]+d [0-9]{1,2}:[0-9]{2}:[0-9]{2}$", df_analysis$perf.clean)
)

if (length(days_idx) > 0) {
  df_analysis[days_idx,
              days := as.numeric(sub("d.*", "", perf.clean))]
  
  df_analysis[days_idx,
              time.only := sub(".*d ", "", perf.clean)]
  
  day.time <- tstrsplit(df_analysis$time.only[days_idx], ":")
  
  df_analysis[days_idx,
              performance.seconds :=
                days * 86400 +
                as.numeric(day.time[[1]]) * 3600 +
                as.numeric(day.time[[2]]) * 60 +
                as.numeric(day.time[[3]])]
}

# inspect unusual performance formats that were not converted
unusual_perf <- unique(
  df_analysis[
    is.na(performance.seconds) & !is.na(athlete.performance),
    perf.clean
  ]
)
head(unusual_perf, 50)

# remove missing / zero performance times
df_analysis <- df_analysis[!is.na(performance.seconds)]
df_analysis <- df_analysis[performance.seconds > 0]

# =========================================
# DISTANCE AND PACE FILTERING
# =========================================

# remove extremely long fixed-distance races > 500 km
df_analysis <- df_analysis[distance.km <= 500]

# compute pace
df_analysis[, pace.min.per.km := (performance.seconds / 60) / distance.km]

summary(df_analysis)
# =========================================
# RAW DATA BOXPLOT BEFORE FILTERING
# =========================================

boxplot(df_analysis$pace.min.per.km,
        data=df_analysis,
        main = "Pace Before Filtering",
        ylab = "Minutes per km")
        


# remove impossible or extreme pace outliers
df_analysis <- df_analysis[pace.min.per.km >= 2 & pace.min.per.km <= 15]

#box plot after filtering
boxplot(df_analysis$pace.min.per.km,
        main = "Cleaned Pace After Filtering",
        ylab = "Minutes per km")
# checks
summary(df_analysis$distance.km)
summary(df_analysis$pace.min.per.km)
summary(df_analysis$performance.seconds)

# =========================================
# EVENT DATE CLEANING
# =========================================

# some dates appear like "05.-07.01.2018"
# keep the starting day
df_analysis[, event.dates.raw := event.dates]
df_analysis[, event.dates := gsub("^([0-9]{2})\\.-[0-9]{2}\\.", "\\1.", event.dates)]

# convert to Date
df_analysis[, event.dates := as.Date(event.dates, format = "%d.%m.%Y")]

# checks
class(df_analysis$event.dates)
sum(is.na(df_analysis$event.dates))
head(df_analysis[is.na(event.dates), .(event.dates.raw, event.name)], 20)

# =========================================
# AGE VARIABLE
# =========================================

summary(df_analysis$athlete.year.of.birth)
sum(is.na(df_analysis$athlete.year.of.birth))

df_analysis[, age := year.of.event - athlete.year.of.birth]

summary(df_analysis$age)

# remove implausible ages
df_analysis <- df_analysis[is.na(age) | (age >= 18 & age <= 75)]

# =========================================
# DISTANCE GROUPS
# =========================================

df_analysis[, distance.group := cut(
  distance.km,
  breaks = c(0, 60, 110, 170, 250, Inf),
  labels = c("~50km", "~100km", "~100mi", "~200km", "200km+")
)]

df_analysis[, .N, by = distance.group]

# =========================================
# GENDER CLEANING
# =========================================

unique(df_analysis$athlete.gender)
sum(is.na(df_analysis$athlete.gender))

df_analysis[athlete.gender == "", athlete.gender := NA_character_]
df_analysis[athlete.gender == "X", athlete.gender := NA_character_]

# =========================================
# COUNTRY CLEANING
# =========================================

df_analysis[, athlete.country := toupper(trimws(athlete.country))]
df_analysis[athlete.country == "", athlete.country := NA_character_]

country_counts <- df_analysis[, .N, by = athlete.country][order(athlete.country)]
head(country_counts, 30)

# =========================================
# ATHLETE AGE CATEGORY CLEANING
# =========================================

unique(df_analysis$athlete.age.category)
df_analysis[athlete.age.category == "", athlete.age.category := NA_character_]

# fix inconsistent prefixes against gender
df_analysis[
  grepl("^W", athlete.age.category) & athlete.gender == "M",
  athlete.age.category := sub("^W", "M", athlete.age.category)
]

# =========================================
# DUPLICATES AND ATHLETE ID CHECKS
# =========================================

sum(duplicated(df_analysis))

# inspect repeated athlete/event/date combos
dup_combo <- df_analysis[, .N, by = .(athlete.id, event.name, event.dates)][N > 1]
head(dup_combo, 20)

# exact duplicate rows
df_analysis <- unique(df_analysis)

# check IDs linked to multiple birth years
df_analysis[, unique.birth.years := uniqueN(athlete.year.of.birth), by = athlete.id]

df_analysis[unique.birth.years > 1, uniqueN(athlete.id)]

bad_ids <- df_analysis[unique.birth.years > 1, unique(athlete.id)]
df_analysis[athlete.id %in% bad_ids, .N]

# =========================================
# EVENT COUNTRY FROM EVENT NAME
# =========================================

df_analysis[, event.country := fifelse(
  grepl("\\([^)]+\\)$", event.name),
  sub(".*\\(([^)]+)\\)$", "\\1", event.name),
  NA_character_
)]

head(unique(df_analysis$event.country), 20)
df_analysis[, .N, by = event.country][order(-N)][1:20]
sum(is.na(df_analysis$event.country))
uniqueN(df_analysis$event.country)

# =========================================
# EVENT NUMBER OF FINISHERS
# =========================================

class(df_analysis$event.number.of.finishers)
sum(is.na(df_analysis$event.number.of.finishers))
summary(df_analysis$event.number.of.finishers)

# remove zero-finisher events
df_analysis <- df_analysis[event.number.of.finishers > 0]

# =========================================
# ATHLETE CLUB
# =========================================

sum(is.na(df_analysis$athlete.club))
df_analysis[athlete.club == "", .N]
uniqueN(df_analysis$athlete.club)

# =========================================
# EVENT SEASON
# =========================================

df_analysis[, event.month := as.integer(format(event.dates, "%m"))]

df_analysis[, event.season :=
              fifelse(event.month %in% c(12, 1, 2), "Winter",
                      fifelse(event.month %in% c(3, 4, 5), "Spring",
                              fifelse(event.month %in% c(6, 7, 8), "Summer",
                                      fifelse(event.month %in% c(9, 10, 11), "Fall", NA_character_))))]

df_analysis[, .N, by = event.season]
sum(is.na(df_analysis$event.season))
sum(is.na(df_analysis$event.dates))

# =========================================
# SIMPLE PACE SUMMARY
# =========================================

distance_pace <- df_analysis[
  !is.na(pace.min.per.km),
  .(
    mean_pace = mean(pace.min.per.km),
    median_pace = median(pace.min.per.km),
    n = .N
  ),
  by = event.distance.length
][order(mean_pace)]

distance_pace

# =========================================
# QUICK PLOT
# =========================================

plot_idx <- sample(nrow(df_analysis), min(nrow(df_analysis), 200000))

plot(df_analysis$distance.km[plot_idx],
     df_analysis$pace.min.per.km[plot_idx],
     pch = 16,
     cex = 0.3,
     xlab = "Distance (km)",
     ylab = "Pace (min/km)")

# =========================================
# NUMERIC CORRELATION CHECK
# =========================================

numeric_vars <- df_analysis[, .(
  distance.km,
  performance.seconds,
  pace.min.per.km,
  age,
  event.number.of.finishers
)]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix

# =========================================
# REMOVE HELPER COLUMNS
# =========================================

cols_to_remove <- intersect(
  c(
    "perf.clean", "has.days", "days", "time.only",
    "distance.main", "event.value", "event.unit",
    "event.dates.raw", "event.month", "unique.birth.years"
  ),
  names(df_analysis)
)

if (length(cols_to_remove) > 0) {
  df_analysis[, (cols_to_remove) := NULL]
}

# =========================================
# SAVE CLEAN DATA
# =========================================

saveRDS(df_analysis,
        "C:/Users/Cailey/OneDrive - University of South Florida/USF/Spring 26/Stats 2/Project/Ultra_Marathon_Stats_Project/Data/ultra_rankings_clean.rds")

fwrite(df_analysis,
       "C:/Users/Cailey/OneDrive - University of South Florida/USF/Spring 26/Stats 2/Project/Ultra_Marathon_Stats_Project/Data/ultra_rankings_clean.csv")

# final checks
dim(df_analysis)
names(df_analysis)
summary(df_analysis)