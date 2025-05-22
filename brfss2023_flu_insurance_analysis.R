# brfss2023_flu_insurance_analysis.R
# Analysis of BRFSS 2023: Flu Shot Uptake, Insurance Coverage, and Demographics
# Author: Asta Musa
# Date: 2025-05-21
# Purpose: Analyze relationship between insurance status and flu shot uptake, stratified by demographic variables

# ===========================
# 1. Load Libraries
# ===========================
library(tidyverse)
library(haven)
library(janitor)
library(broom)
library(caret)
library(pscl)
library(knitr)

# ===========================
# 2. Data Acquisition & Import
# ===========================
# Prompt user to select file interactively, improving portability
file_path <- file.choose()  
brfss_data <- read_xpt(file_path)

# ===========================
# 3. Data Wrangling & Cleaning
# ===========================
# Select relevant variables
brfss_subset <- brfss_data %>%
  select(
    PRIMINS1, CHECKUP1, FLUSHOT7, SEXVAR, `_AGE80`, EDUCA, INCOME3, `_RACEGR3`
  )

# Remove invalid codes as per BRFSS documentation
brfss_clean <- brfss_subset %>%
  filter(
    !PRIMINS1 %in% c(7, 9),
    !CHECKUP1 %in% c(7, 9),
    !FLUSHOT7 %in% c(7, 9),
    !SEXVAR %in% c(7, 9),
    !EDUCA %in% c(9),
    !INCOME3 %in% c(77, 99),
    !is.na(`_AGE80`)
  ) %>%
  mutate(
    insured = if_else(PRIMINS1 == 1, "Yes", "No"),
    checkup_recent = if_else(CHECKUP1 == 1, "Within past year", "Longer ago"),
    flu_shot = if_else(FLUSHOT7 == 1, "Yes", "No"),
    sex = factor(SEXVAR, levels = c(1, 2), labels = c("Male", "Female")),
    age = `_AGE80`,
    education = factor(EDUCA),
    income = factor(INCOME3),
    race_group = factor(`_RACEGR3`)
  )

# ===========================
# 4. Descriptive Statistics
# ===========================
# Frequency tables
tabyl(brfss_clean, insured)
tabyl(brfss_clean, flu_shot)
tabyl(brfss_clean, checkup_recent)
tabyl(brfss_clean, sex)
tabyl(brfss_clean, race_group)
tabyl(brfss_clean, education)
tabyl(brfss_clean, income)

# Cross-tabulations
brfss_clean %>%
  tabyl(flu_shot, insured) %>%
  adorn_percentages("row") %>%
  adorn_totals("row") %>%
  adorn_title()

brfss_clean %>%
  tabyl(checkup_recent, insured) %>%
  adorn_percentages("row") %>%
  adorn_totals("row") %>%
  adorn_title()

# ===========================
# 5. Visual Explorations
# ===========================
# Flu shot by income
ggplot(brfss_clean, aes(x = income, fill = flu_shot)) +
  geom_bar(position = "fill") +
  labs(title = "Flu Shot Uptake by Income Level", y = "Proportion", x = "Income", fill = "Flu Shot") +
  coord_flip() +
  theme_minimal()

# Checkup by education
ggplot(brfss_clean, aes(x = education, fill = checkup_recent)) +
  geom_bar(position = "fill") +
  labs(title = "Routine Checkup by Education Level", y = "Proportion", x = "Education", fill = "Checkup") +
  coord_flip() +
  theme_minimal()

# Age distribution by flu shot
ggplot(brfss_clean, aes(x = age, fill = flu_shot)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Age Distribution by Flu Shot Status", x = "Age", y = "Count") +
  theme_minimal()

# ===========================
# 6. Modeling: Logistic Regression
# ===========================
brfss_model <- brfss_clean %>%
  drop_na(flu_shot, insured, age, sex, income, education, race_group) %>%
  mutate(flu_shot_binary = if_else(flu_shot == "Yes", 1, 0))

flu_model <- glm(flu_shot_binary ~ insured + age + sex + income + education + race_group,
                 data = brfss_model, family = "binomial")

summary(flu_model)
pR2(flu_model)

# ===========================
# End of Analysis
# ===========================
