library(haven)
library(data.table)
library(dplyr)
library(ggplot2)

# read bp dataset
bp <- read_xpt("BPXO_L.xpt")
bmi <- read_xpt("BMX_L.xpt")
demo <- read_xpt("DEMO_L.xpt")

# merge datasets
nhanes <- merge(bp, bmi, by = "SEQN", all = TRUE)
nhanes <- merge(nhanes, demo, by = "SEQN", all = TRUE)
nhanes_dt <- setDT(nhanes)

# Create average BP
nhanes_dt[, mean_sbp := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("BPXOSY1", "BPXOSY2", "BPXOSY3")]

summary(nhanes_dt$mean_sbp, na.rm = TRUE)

# Create age group
summary(nhanes_dt$RIDAGEYR, na.rm = TRUE)
nhanes_dt[, age_gp := cut(
  RIDAGEYR, 
  breaks = c(0, 8, 18, 40, 60,Inf), 
  labels = c("0-8", "9–17", "18–39", "40–59", "60+"),
  right = FALSE
)]
table(nhanes_dt$age_gp, useNA = "always")

# Limit the dataset to >8 years old because BP is not measured for children <8 years old. 
nhanes_final <- nhanes_dt[age_gp != "0-8"]

# Diagnose missing percent of SBP by age
bp_missing_by_age <- nhanes_final %>%
  group_by(age_gp) %>%
  summarise(
    n = n(),
    missing_percent = round(mean(is.na(mean_sbp)) * 100, 2)
  )

## Plotting 
# SBP by BMI plot
bp_bmi_plot <- ggplot(nhanes_final, aes(x = BMXBMI, y = mean_sbp)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linewidth = 1) + 
  labs(
    title = "Systolic Blood Pressure by Body Mass Index", 
    x = "Body Mass Index (kg/m²)", 
    y = "Mean SBP (mmHg)"
  ) 

# SBP by BMI across age groups plot
bp_bmi_age_plot <- ggplot(nhanes, aes(x = BMXBMI, y = mean_sbp)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linewidth = 1) + 
  facet_wrap(~ age_gp, nrow = 2) +
  labs(
    title = "Systolic Blood Pressure by Body Mass Index by Age Group", 
    x = "Body Mass Index (kg/m²)", 
    y = "Mean SBP (mmHg)"
  ) 

# Diagnostics: plotting SBP and BMI by age separately
bp_age_plot <- ggplot(nhanes_final, aes(x = RIDAGEYR, y = mean_sbp)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linewidth = 1) + 
  labs(
    title = "Systolic Blood Pressure by Age",
    x = "Age (Years)",
    y = "Mean SBP (mmHg)"
  )

## Modelling
# Raw model - SBP on BMI

# Adjusted model - SBP on BMI adjusted by age group 

# Linear Mixed Effect Model

