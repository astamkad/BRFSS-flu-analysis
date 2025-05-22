# BRFSS 2023 Flu Shot & Insurance Status Analysis

This project explores the relationship between insurance status and preventive health behavior, specifically flu shot uptake, using the 2023 Behavioral Risk Factor Surveillance System (BRFSS) dataset.

## Data Source
- **Source**: [CDC BRFSS 2023 Data](https://www.cdc.gov/brfss/annual_data/annual_2023.html)
- **File**: `LLCP2023.XPT` (fixed-width .XPT format provided by the CDC)

## Key Research Questions
- Does insurance coverage influence preventive health behaviors such as flu vaccination and routine checkups?
- Are there demographic patterns by sex, age, education, income, or race?

## Objective
Analyze flu shot uptake based on:
- Insurance coverage
- Routine checkup history
- Age
- Income
- Education
- Race and sex

## Tools & Packages
- tidyverse, haven, janitor, broom, caret, pscl, ggplot2

## File Structure
- `brfss2023_flu_insurance_analysis.R`: Script with all steps from import to logistic regression
- `brfss2023_flu_insurance_analysis.qmd`: Renderable Quarto report
- `.gitignore`: Git configuration
- `README.md`: This is a documentation file


## Key Steps
1. **Data Import & Cleaning**
   - BRFSS `.XPT` file loaded using `haven::read_xpt()`
   - Cleaned and filtered using `dplyr`, `janitor`, and `tidyverse` tools

2. **Exploratory Analysis**
   - Frequency tables
   - Cross-tabulations (e.g., flu shot by insurance)
   - Bar plots and histograms

3. **Logistic Regression**
   - Predictors: insurance, age, income, education, race, sex
   - Outcome: flu shot uptake
   - Evaluated with pseudo R², confusion matrix, accuracy


## Key Findings
   - Among those who received a flu shot, 61% were insured, while 39% were uninsured..
   - Preventive care usage correlates with education and income.
   - Logistic regression confirms insurance status as a statistically significant predictor of flu shot uptake (p < 0.001).

   These results reinforce the role of access to insurance in supporting public health outcomes.


## Libraries Used
```r
library(tidyverse)
library(haven)
library(janitor)
library(broom)
library(caret)
library(pscl)
