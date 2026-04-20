# Ultra Marathon Running Analysis

> Exploring 224 years of endurance — performance, demographics, and race trends from 1798 to 2022.

Ultramarathons are any foot race exceeding the standard marathon distance of 42.195 km.
They range from 50 km trail runs to multi-day fixed-time events (e.g., 24-hour or 6-day races),
attracting athletes who push well beyond conventional limits of human endurance.

---

## Dataset

| Metric | Value |
|---|---|
| Total race records | 7,461,226 |
| Unique athletes | 1,641,168 |
| Time span | 1798 – 2022 |

### Variables

**Event information**
- Year, date, and full event name
- Race distance or type (distance-based vs. time-based)
- Number of finishers per event

**Athlete information**
- Anonymized unique athlete ID (for longitudinal tracking)
- Country of origin
- Gender
- Age at time of race
- Club affiliation

**Performance metrics**
- Finish time
- Average speed (km/h)

> [!NOTE]
> Athletes are identified using anonymized IDs to enable multi-race longitudinal analysis
> while fully preserving privacy.

---

## Objectives

This project addresses four core research questions:

1. **Performance over time** — Have finish times and average speeds improved across decades?
   Are elite and mid-pack athletes trending faster?

2. **Gender and age comparisons** — How do performance profiles differ across genders and age groups?
   At what age do athletes typically peak, and how does speed decline afterward?

3. **Race type analysis** — Do distance-based and time-based events attract different athlete profiles?
   How do completion rates and average speeds compare between formats?

4. **Speed determinants** — Which factors — age, gender, race type, season, location, experience —
   are the strongest predictors of average speed in a regression framework?

---

## Methods

### 1. Data Cleaning & Preprocessing
- Handling missing and malformed values across 7.4M records
- Standardizing date formats and converting variable types
- Identifying and resolving duplicates

### 2. Feature Engineering
- **Athlete age** at the time of each race
- **Seasonality** (Q1–Q4) derived from event dates
- **Location** extracted and categorized from event names

### 3. Descriptive Statistics & Visualization
- Distribution plots of speed and finish time by gender, age group, and race type
- Trend lines showing performance changes across decades
- Participation growth over time

### 4. Inferential Statistics & Regression
- Hypothesis testing to compare group performance (e.g., gender, age brackets)
- Linear and/or multiple regression to quantify the effect of key variables on average speed

> [!NOTE]
> Due to dataset size (~7.5M records), some analyses are performed on
> representative subsets to ensure computational efficiency without sacrificing accuracy.

---

## Project Files

| File | Description |
|---|---|
| `ultra_marathon_data_cleaning_log.xlsx` | Step-by-step documentation of all data cleaning decisions |
| `Questions and Methods.xlsx` | Full list of research questions and planned statistical methods |

---

## Acknowledgments

Data sourced from publicly available ultra-marathon race records spanning 1798–2022.
All athlete data has been fully anonymized. No personally identifiable information is
included in any analysis or output.