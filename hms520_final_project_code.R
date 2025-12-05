### HMS520 final project: Autumn 2025
### Submitted by Zaw Wai Yan Bo and Pyone Yadanar Paing
### Last updated by 12/4/2025
## Load libraries 
library(haven)
library(data.table)
library(dplyr)
library(ggplot2)
library(rlang)
library(lme4)

### Data Preparation and cleaning
## Datasets are downloaded from website below. 
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2021-2023
## Read datasets
bp <- read_xpt("BPXO_L.xpt")
bmi <- read_xpt("BMX_L.xpt")
demo <- read_xpt("DEMO_L.xpt")

# merge datasets
nhanes <- merge(merge(bp, bmi, by = "SEQN", all = TRUE),
                demo, by = "SEQN", all = TRUE)
nhanes_dt <- setDT(nhanes)
View(nhanes_dt)
# Create average BP

# Limit the dataset to 20+ years old because we are using adult BMI categories
nhanes_dt <- nhanes_dt[RIDAGEYR >= 20]

# Create average BP by using all available BP measurements
nhanes_dt[, mean_sbp := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("BPXOSY1", "BPXOSY2", "BPXOSY3")]

# Create BMI categories
nhanes_dt[, bmi_cat := ifelse(BMXBMI < 18.5, "Underweight",
                      ifelse(BMXBMI < 25, "Healthy weight",
                      ifelse(BMXBMI < 30, "Overweight",
                      "Obesity")))]

# Factoring bmi_cat variable
nhanes_dt[, bmi_cat := factor(bmi_cat,
                              levels = c("Underweight",
                                         "Healthy weight",
                                         "Overweight",
                                         "Obesity"),
                              ordered = TRUE)]

# Create age group
summary(nhanes_dt$RIDAGEYR, na.rm = TRUE)
nhanes_dt[, age_gp := cut(
  RIDAGEYR, 
  breaks = c(20, 35, 50, 65, Inf), 
  labels = c("20–34", "35–49", "50-64", "65+"),
  right = FALSE
)]
table(nhanes_dt$age_gp, useNA = "always")

# Diagnose missing percent of SBP, BMI and age
bp_bmi_age_missing <- nhanes_dt %>%
  summarise(
    total_n = n(),
    
    # Count: all SBP, BMI and age measurements missing
    missing_n = sum(is.na(mean_sbp) | is.na(BMXBMI) | is.na(RIDAGEYR)),
    
    # Percentages
    missing_percent = round(missing_n / total_n * 100, 2)
  )

# Limit the dataset to observations with non-missing rows for SBP, BMI and Age
nhanes_final <- nhanes_dt[
  !is.na(mean_sbp) &
    !is.na(BMXBMI) &
    !is.na(RIDAGEYR)
]

### Summary statistics and Plotting 
## Histogram to see distribution of BMI and SBP by age
# Function to create histogram by facet variable
hist_plot <- function(data, xvar, facet_var, binwidth = 1,
                      fill = "skyblue", color = "black") {
  
  ggplot(data, aes_string(x = xvar)) +
    geom_histogram(binwidth = binwidth, fill = fill, color = color) +
    facet_wrap(as.formula(paste("~", facet_var)))
}

sbp_age_hist <- hist_plot(nhanes_final, "mean_sbp", "age_gp") + 
  labs(
    title = "Histogram of Systolic Blood Pressure by Age", 
    x = "Systolic Blood Pressure (mmHg)", 
    y = "Frequency"
  )

bmi_age_hist <- hist_plot(nhanes_final, "BMXBMI", "age_gp") + 
  labs(
    title = "Histogram of Body Mass Index by Age", 
    x = "Body Mass Index (kg/m²)", 
    y = "Frequency"
  )

sbp_bmi_hist <- hist_plot(nhanes_final, "mean_sbp", "bmi_cat") + 
  labs(
    title = "Histogram of Systolic Blood Pressure by BMI", 
    x = "Systolic Blood Pressure (mmHg)", 
    y = "Frequency"
  )

## Write function for scatter plots
plot_xy <- function(data, xvar, yvar, facet_var = NULL) {
  # Convert character variable names to symbols
  x <- sym(xvar)
  y <- sym(yvar)
  
  p <- ggplot(data, aes(x = !!x, y = !!y)) +
    geom_point(alpha = 0.6) +   
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = paste(yvar, "vs", xvar),
      x = xvar,
      y = yvar
    )
  
  # Optional facet
  if (!is.null(facet_var)) {
    facet_sym <- sym(facet_var)
    p <- p + facet_wrap(vars(!!facet_sym))
  }
  
  return(p)
}

# SBP by BMI plot
sbp_bmi_plot <- plot_xy(nhanes_final, "BMXBMI", "mean_sbp") + labs(
  title = "Systolic Blood Pressure by BMI", 
  x = "Body Mass Index (kg/m²)", 
  y = "Systolic Blood Pressure (mmHg)"
) 

# SBP by BMI across age groups plot
sbp_bmi_age_plot <- plot_xy(nhanes_final, "BMXBMI", "mean_sbp", 
                            facet_var = "age_gp") +
  labs(
    title = "Systolic Blood Pressure by BMI across Age Groups", 
    x = "Body Mass Index (kg/m²)", 
    y = "Systolic Blood Pressure (mmHg)"
  ) 

## Modelling 
# Simple Linear Regression Model
lm_model <- lm(mean_sbp~BMXBMI, data = nhanes_final)
summary(lm_model)

# Linear Regression Model adjusted for age
lm_model_adjusted <- lm(mean_sbp~BMXBMI+RIDAGEYR, data = nhanes_final)
summary(lm_model_adjusted)

## Modelling as Group-specific Regression 
# split the dataset by age group
nhanes_group <- split(nhanes_final, nhanes_final$age_gp)

# Model SBP on BMI
models <- lapply(nhanes_group, function(dt) lm(mean_sbp ~ BMXBMI, data = dt))

for (key in names(nhanes_group)) {
  nhanes_group[[key]][, mean_sbp_fit := predict(models[[key]], 
                                                nhanes_group[[key]])]
}

# Recombine into main dataset
nhanes_final <- rbindlist(nhanes_group)

# Plot fit
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

# View summary of models 
lapply(models, summary)