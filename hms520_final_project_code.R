library(haven)
library(data.table)
library(dplyr)
library(ggplot2)
library(lme4)

# read bp dataset
bp <- read_xpt("BPXO_L.xpt")
bmi <- read_xpt("BMX_L.xpt")
demo <- read_xpt("DEMO_L.xpt")

# merge datasets
nhanes <- merge(bp, bmi, by = "SEQN", all = TRUE)
nhanes <- merge(nhanes, demo, by = "SEQN", all = TRUE)
nhanes_dt <- setDT(nhanes)
View(nhanes_dt)
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


# SBP by BMI plot
bp_bmi_plot1 <- ggplot(nhanes_final, aes(x = BMXBMI)) + 
  geom_point(aes(y = mean_sbp, color = age_gp)) +
  geom_smooth(method = "lm", se = FALSE, color = "#E74C3C", linewidth = 1) +
  labs(
    title = "Systolic Blood Pressure by Body Mass Index", 
    x = "Body Mass Index (kg/m²)", 
    y = "Mean SBP (mmHg)"
  ) 

 

## Modelling as Group-specific Regression 
# remove the 0-8 age group
nhanes_final$age_gp <- droplevels(nhanes_final$age_gp)

nhanes_group <- split(nhanes_final, nhanes_final$age_gp)

# Model SBP on BMI
models <- lapply(nhanes_group, function(dt) lm(mean_sbp ~ BMXBMI, data = dt))

for (key in names(nhanes_group)) {
  nhanes_group[[key]][, mean_sbp_fit := predict(models[[key]], 
                                                nhanes_group[[key]])]
}

# Recombine into main dataset
nhanes_final <- rbindlist(nhanes_group)

# plot fit
nhanes_final <- nhanes_final[order(age_gp, BMXBMI)]
ggplot(nhanes_final, aes(x = BMXBMI)) +
  geom_point(aes(y = mean_sbp), alpha = 0.3) +
  geom_line(aes(y = mean_sbp_fit, color = age_gp), linewidth = 1) +
  facet_wrap(~ age_gp) +
  labs(
    title = "Group-Specific Regression: SBP vs BMI by Age Group",
    x = "BMI",
    y = "Observed and Fitted SBP"
  )

sapply(models, function(m) coef(m)[["BMXBMI"]])




# Adjusted model - SBP on BMI adjusted by age group 

# Linear Mixed Effect Model