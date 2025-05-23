# brfss2023_flu_insurance_analysis.R
# Analysis of BRFSS 2023: Flu Shot Uptake, Insurance Coverage, and Demographics
# Author: Asta Musa
# Date: 2025-05-21
# Purpose: Analyze relationship between insurance status and flu shot uptake, stratified by demographic variables
# Data Source: 2023 Behavioral Risk Factor Surveillance System (BRFSS), CDC.

# ===========================
# Load Libraries
# ===========================
library(tidyverse)
library(haven)
library(janitor)
library(broom)
library(caret)
library(pscl)
library(knitr)

# ===========================
# 1. Data Acquisition & Import
# ===========================
# Prompt user to select file interactively, improving portability
file_path <- file.choose()  
brfss_data <- read_xpt(file_path)

# ===========================
# 2. Data Wrangling & Cleaning
# ===========================
# Step 1: Select a subset of the relevant variables for the analysis
brfss_subset <- brfss_data %>%
  select(
    PRIMINS1,       # Insurance status
    CHECKUP1,       # Time since last routine checkup
    FLUSHOT7,       # Flu shot in past 12 months
    SEXVAR,         # Biological sex
    `_AGE80`,       # Age (top-coded at 80+)
    EDUCA,          # Education level
    INCOME3,        # Income category
    `_RACEGR3`      # Race/ethnicity group
  )

# Step 2: Explore missing and invalid codes
summary(brfss_subset)
colSums(is.na(brfss_subset))
lapply(brfss_subset, unique)

# Step 3: Filter out invalid survey codes (e.g., 7, 9, 77, 99)
brfss_filtered <- brfss_subset %>%
  filter(
    !PRIMINS1 %in% c(7, 9),
    !CHECKUP1 %in% c(7, 9),
    !FLUSHOT7 %in% c(7, 9),
    !SEXVAR %in% c(7, 9),
    !EDUCA %in% c(9),
    !INCOME3 %in% c(77, 99),
    !is.na(`_AGE80`)
  )

# Step 4: Recode variables into readable factors and labels
brfss_clean <- brfss_filtered %>%
  mutate(
    insured = factor(if_else(PRIMINS1 == 1, "Yes", "No"), levels = c("No", "Yes")),
    checkup_recent = factor(if_else(CHECKUP1 == 1, "Within past year", "Longer ago")),
    flu_shot = factor(if_else(FLUSHOT7 == 1, "Yes", "No")),
    sex = factor(SEXVAR, levels = c(1, 2), labels = c("Male", "Female")),
    age = `_AGE80`,
    education = factor(EDUCA, levels = 1:6, labels = c("Never attended", "Grades 1-8", "Grades 9-11", "High school", "Some college", "College graduate")),
    income = factor(INCOME3),
    race_group = factor(`_RACEGR3`)
  )

names(brfss_clean)
colSums(is.na(brfss_clean))

# ===========================
# 3. Exploratory Data Analysis
# ===========================
# ---- Step 1: Categorical Distributions ----

insured_tab <- brfss_clean %>% tabyl(insured)
flu_shot_tab <- brfss_clean %>% tabyl(flu_shot)
checkup_tab <- brfss_clean %>% tabyl(checkup_recent)
sex_tab <- brfss_clean %>% tabyl(sex)
race_tab <- brfss_clean %>% tabyl(race_group)
edu_tab <- brfss_clean %>% tabyl(education)
income_tab <- brfss_clean %>% tabyl(income)

# ---- Step 2: Cross-Tabulations ----

flu_by_ins <- brfss_clean %>%
  tabyl(flu_shot, insured) %>%
  adorn_percentages("row") %>%
  adorn_totals("row") %>%
  adorn_title()

checkup_by_ins <- brfss_clean %>%
  tabyl(checkup_recent, insured) %>%
  adorn_percentages("row") %>%
  adorn_totals("row") %>%
  adorn_title()

# ---- Step 3: Visual Explorations ----

plot_flu_income <- brfss_clean %>%
  ggplot(aes(x = income, fill = flu_shot)) +
  geom_bar(position = "fill") +
  labs(
    title = "Flu Shot Uptake by Income Level",
    y = "Proportion",
    x = "Income Category",
    fill = "Flu Shot"
  ) +
  theme_minimal() +
  coord_flip()

plot_checkup_edu <- brfss_clean %>%
  ggplot(aes(x = education, fill = checkup_recent)) +
  geom_bar(position = "fill") +
  labs(
    title = "Routine Checkup by Education Level",
    y = "Proportion",
    x = "Education Level",
    fill = "Checkup Timing"
  ) +
  theme_minimal() +
  coord_flip()

plot_age_flu <- brfss_clean %>%
  ggplot(aes(x = age, fill = flu_shot)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.6) +
  labs(
    title = "Age Distribution by Flu Shot Status",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

# ---- Step 4: Display Outputs ----

insured_tab
flu_shot_tab
checkup_tab
sex_tab
race_tab
edu_tab
income_tab

flu_by_ins
checkup_by_ins

plot_flu_income
plot_checkup_edu
plot_age_flu

# ===========================
# 4. Modeling: Logistic Regression
# ===========================
# ---- Step 1: Prepare Data ----

# Drop rows with missing values in model variables
brfss_model <- brfss_clean %>%
  drop_na(flu_shot, insured, age, sex, income, education, race_group)

# ---- Step 2: Fit Logistic Regression Model ----

# Fit logistic regression model
flu_model <- glm(
  flu_shot ~ insured + age + sex + income + education + race_group,
  data = brfss_model,
  family = "binomial"
)

# ---- Step 3: Summarize Model ----

# Tidy model summary with odds ratios and 95% confidence intervals
flu_model_summary <- tidy(flu_model, exponentiate = TRUE, conf.int = TRUE)

# View model output
flu_model_summary

# === 5. Statistical Modeling & Interpretation ===
# Objective: Use logistic regression to model flu shot uptake based on insurance and demographics
# Step 1: Prepare Data
# Filter to keep only complete cases for modeling variables
brfss_model <- brfss_clean %>%
  drop_na(flu_shot, insured, age, sex, income, education, race_group)

# Recode 'flu_shot' as a binary outcome: Yes = 1, No = 0
brfss_model <- brfss_model %>%
  mutate(flu_shot_binary = if_else(flu_shot == "Yes", 1, 0))

# Step 2: Fit Logistic Regression Model
# Model flu shot uptake based on insurance, age, sex, income, education, and race
flu_model <- glm(
  flu_shot_binary ~ insured + age + sex + income + education + race_group,
  data = brfss_model,
  family = "binomial"
)

# Step 3: Summarize Model Output
summary(flu_model)

# Step 4: Generate Odds Ratios
exp(coef(flu_model))

# Step 5: Tidy summary for presentation
tidy(flu_model, exponentiate = TRUE, conf.int = TRUE) %>%
  arrange(p.value) %>%
  knitr::kable(digits = 3, caption = "Logistic Regression Results (Odds Ratios)")

# Step 6: Model Evaluation
# Pseudo R-squared to evaluate model fit
pR2(flu_model)

# ===========================
# End of Analysis
# ===========================
