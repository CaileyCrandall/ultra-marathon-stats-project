# Ultra Marathon Data Analysis

> What factors influence ultramarathon pacing?
> Using 6.8 million race records from 1798–2022.

Ultramarathons are foot races exceeding the standard marathon distance (42.195 km).
This project investigates which factors — distance, gender, age, season, and location —
predict athlete pace (min/km) using a large historical dataset spanning over 200 years.

---

## Dataset

| Metric | Value |
|---|---|
| Raw records (before cleaning) | 6,837,563 |
| Time span | 1798 – 2022 |

### Variables

| Category | Fields |
|---|---|
| Event | Year, date, distance, country, finishers |
| Athlete | Gender, age |
| Performance | Time, speed |
| Derived | Pace (min/km), distance (km), seconds |

---

## Method

The analysis follows a five-stage pipeline:

**1. Data cleaning** → **2. EDA** → **3. Hypothesis testing** → **4. Two-Way ANOVA** → **5. Multiple linear regression + model evaluation**

---

## Key highlights

- Pace mostly falls between 6–10 min/km; athletes are typically 35–45 years old
- Most common distances are 50 km, 100 km, and 100 miles
- Distance is the strongest predictor of pace — longer races produce slower times
- Men average faster paces than women, though the gap narrows at longer distances
- Country/location explains substantial variation in pace beyond individual athlete traits
- Raw seasonal trends can be misleading without controlling for event location

> [!NOTE]
> For full statistical results, model coefficients, and visualizations, see the
> analysis scripts in the **Project Files** section below.

---

## Limitations

**Unknown factors** — diet, race elevation, overall athlete fitness, and weather conditions are not captured in the dataset.

**Observational data** — this is not experimental; causal claims cannot be made.

**Large sample size** — with ~6.8 million records, even tiny effects become statistically significant and p-values alone should not be over-interpreted.

---

## Project Files

| File | Description |
|---|---|
| `01.1_data_cleaning.R` | Standardizes raw race data, handles missing values, filters outliers, and engineers derived variables |
| `02-pre_analysis.R` | Exploratory data analysis — generates distribution plots for pace, age, distance, and season |
| `03-ANOVA.R` | One-way and two-way ANOVA testing the effects of gender and distance group on pace |
| `Final model.R` | Builds and evaluates a multiple linear regression model predicting athlete pace |
---

*Data sourced from publicly available ultra-marathon race records. All athlete data is anonymized.*