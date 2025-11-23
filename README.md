# HMS520_final_project
This is the repository for final group project of HMS520-Autumn2025 submitted by Zaw Wai Yan Bo and Pyone Yadanar Paing.

# Format of the analysis 
We will analyze National Health and Nutrition Examination Survey (NHANES) dataset that uses data wrangling and modeling tools in R. 

# Overall goal of the project
The overall goal of this final project is to model mean systolic blood pressure (mmHg) by body mass index (kg/m2) among children who are 8 years old and above and adults in the United States by using NHANES (2021-2023) dataset. 

# Detailed plans and timelines
# Week 1: Proposla development, Dataset Preparation, Data Cleaning and Variable Creation
# 1: Download and load datasets
Download three datasets necessary for this analyis from NHANES website: BMI, BP, Demographic files. 
Merge those three datasets by respondent ID. 

# 2: Data Cleaning
Subset the data for individuals 8 years and older
Clean the variable names.
Review and handle the missing values for BMI and BP

# 3: Create necessary variables
Create a mean SBP (mean_SBP) variable by taking average of three SBP measurements. 
Create a categorical age variable.
Create a categorical BMI variable. ???
Model systolic blood pressure as dependent variable by body mass index as independent variable by age group.  

We will use data.table package, ggplot2 packages. 

# Week 2:
# 4: Plotting and Summary Statistics
Summary tables for Age, BMI, SBP
Create histograms for BMI and SBP
Create boxplots of SBP by BMI
Create scatter plot of SBP by BMI
Explore trends, outliers, associations

# 5: Modelling
Fit Linear mixed-effects model and check assumptions
model <- SBP ~ (1 | Age) + BMI

# 6: Report Model Findings, interpretation and PowerPoint preparation
Interpret model results in plain language.
Summarize key findings, limitations, and insights.
Produce final plots and tables.
Submit project via GitHub repository with script
Finalize presentation slides 

# Week 3: Presentation
