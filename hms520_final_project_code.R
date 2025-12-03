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
View(nhanes_dt)

nhanes_dt[, mean_sbp := rowMeans(.SD, na.rm = TRUE), 
          .SDcols = c("BPXOSY1", "BPXOSY2", "BPXOSY3")]

summary(nhanes_dt$mean_sbp, na.rm = TRUE)

bp_missing_by_bmi <- nhanes_dt %>%
  group_by(BMXBMI) %>%
  summarise(
    n = n(),
    missing_bp = sum(is.na(mean_sbp)),
    missing_percent = round(mean(is.na(mean_sbp)) * 100, 2)
  )