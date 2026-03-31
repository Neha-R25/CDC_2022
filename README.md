# CDC Smoking Gender Analysis (BRFSS 2022)

## Research Question
Is gender associated with current smoking status among adults in United States?

## Dataset
Behavioral Risk Factor Surveillance System (BRFSS) 2022 dataset from the CDC.

## Variables

Exposure: Sex (Male, Female)
Outcome: Current smoking status

Smoking status was derived by combining two survey variables:

SMOKE100 — lifetime smoking history
SMOKDAY2 — current smoking frequency

Current smokers were defined as individuals who reported smoking every day or some days.

## Methods
Cleaned sex variable (CELLSEX1) and removed non-response categories.
Created binary smoking indicator (current smoker vs non-current smoker).
Performed cross-tabulation and Chi-square test to evaluate association.
Conducted logistic regression to estimate the odds of smoking by sex.
Converted regression coefficients to odds ratios with 95% confidence intervals.

## Results

Chi-square test showed a significant association between sex and smoking status:

X² = 200.19, df = 1, p < 0.001

Logistic regression results:

Odds Ratio (Male vs Female) = 1.16

95% CI = 1.137 – 1.185

##Interpretation

Males had 16% higher odds of current smoking compared with females, suggesting a statistically significant gender disparity in smoking prevalence.

## Files in this repository

CDC_smoking_gender_analysis2022.R  
R script containing the full analysis.

confidence_intervals.csv  
Confidence intervals for the logistic regression model.

odds_ratio_results.csv  
Odds ratios derived from the logistic regression model.

## Software Used
R  
tidyverse  
haven
