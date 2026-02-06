# Week 1: Initial Data Exploration ====
# Author: [Julia Deb]
# Date: [30/01/2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)

# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   Data collected on 200 female mosquitoes, age recorded in days, and the number of eggs laid and hatched 
# - What's being measured?
#   Depending on the dosage of treatment a mosquito receives if number of eggs laid and hatched changes, control sample was also taken of which no treatment was given to
# - How many observations?
#   205 observations, 9 variables 
# - Anything surprising?
#   Not sure what treatment was given to mosquitoes - what is meant by low and high dosage 
# - Any obvious problems?
#   There is inconsistency in naming of categories e.g high_dose and HIGH_DOSE - make sure they are all in lower case 
#   Also an inconsistency in naming of sites: Site_A , site a 
#   Missing data - decide whether to include those sets of data or not to include it 
#   Impossible data value: body mass being a negative value - possibly a typing error

# FIX 1: Negative Body Mass Values =================================================================================

# Show the problem:
summary(mosquito_egg_raw$body_mass_mg)
#The minimum value is a negative value which is biologically impossible. This will affect any analysis and skew any results. 

# Fix it:
mosquito_egg_raw |> mutate(body_mass_mg = abs(body_mass_mg)) -> mosquito_egg_raw_cleaned  
#Used absolute to change all negative values to positive values

# Verify it worked:
summary(mosquito_egg_raw_cleaned$body_mass_mg)
#Minimum value no longer negative

# What changed and why it matters:
#Now all values in body mass is positive, which means that its biologically plausible.


# FIX 2: Missing Data Values  ==============================================================================

# Show the problem:
mosquito_egg_raw |> 
  filter(if_any(everything(), is.na)) |>
  select(female_id, age_days, body_mass_mg, 
         site, collection_date, collector,treatment,eggs_laid, eggs_hatched,
         everything())

mosquito_egg_raw |> 
  filter(if_any(body_mass_mg, is.na))
#15 missing body mass data

mosquito_egg_raw |> 
  filter(if_any(collector, is.na))
#13 missing collector data
#Collector information does not effect any analysis or results - can leave this data as NA 

mosquito_egg_raw |> 
  filter(if_any(eggs_laid, is.na))
#16 missing number of eggs laid data

mosquito_egg_raw |> 
  filter(if_any(eggs_hatched, is.na))
#17 missing number of eggs hatched data

# Fix it:
library(tidyr)

mosquito_egg_raw_cleaned <- mosquito_egg_raw |> 
  drop_na(eggs_laid) |> 
  drop_na(eggs_hatched) |> 
  drop_na(body_mass_mg) 
  

# Verify it worked:
mosquito_egg_raw_cleaned |> 
  filter(if_any(everything(), is.na)) |>
  select(body_mass_mg, eggs_laid, eggs_hatched,
         everything())

View(mosquito_egg_raw_cleaned)

# What changed and why it matters:
#Dropped NA values for body mass, number of eggs laid and hatched. 

# FIX 3: Standardize Category Values - Treatment  ================================================================================ 

# Show the problem:
mosquito_egg_raw |>
  distinct(treatment)
# There a multiple different cases for the variables within treatment.
# 1 Medium_dose
# 2 High_dose  
# 3 high_dose  
# 4 Low_dose   
# 5 Control    
# 6 HIGH_DOSE  
# 7 MEDIUM_DOSE
# 8 low_dose   
# 9 control    
# 10 LOW_DOSE   
# 11 CONTROL    
# 12 medium_dose

# Fix it:
mosquito_egg_raw_cleaned <- mosquito_egg_raw |>
  mutate(treatment = case_when(
    treatment %in% c("CONTROL" , "Control") ~ "control",  #%in% for vectors to be used to convert
    treatment  %in% c("High_dose", "HIGH_DOSE") ~ "high_dose", 
    treatment  %in% c("Low_dose", "LOW_DOSE") ~ "low_dose", 
    treatment  %in% c("Medium_dose", "MEDIUM_DOSE") ~ "medium_dose",
    .default = as.character(treatment)
  ))

# Verify it worked:
mosquito_egg_raw_cleaned |>
  distinct(treatment)
# 1 medium_dose
# 2 high_dose  
# 3 low_dose   
# 4 control

# What changed and why it matters:
# It changed because a vector was created containing all the variables which then converted each case into 
# the desired syntax. %in% was need to read the vector. This is important as otherwise when using the variable
# of treatment, there would be 12 categories instead of 4.

# FIX 3: Standardize Category Values - Treatment  ================================================================================ 

# Show the problem:
mosquito_egg_raw |>
  distinct(treatment)
# There a multiple different cases for the variables within treatment.
# 1 Medium_dose
# 2 High_dose  
# 3 high_dose  
# 4 Low_dose   
# 5 Control    
# 6 HIGH_DOSE  
# 7 MEDIUM_DOSE
# 8 low_dose   
# 9 control    
# 10 LOW_DOSE   
# 11 CONTROL    
# 12 medium_dose

# Fix it:
mosquito_egg_raw_cleaned <- mosquito_egg_raw |>
  mutate(treatment = case_when(
    treatment %in% c("CONTROL" , "Control") ~ "control",  #%in% for vectors to be used to convert
    treatment  %in% c("High_dose", "HIGH_DOSE") ~ "high_dose", 
    treatment  %in% c("Low_dose", "LOW_DOSE") ~ "low_dose", 
    treatment  %in% c("Medium_dose", "MEDIUM_DOSE") ~ "medium_dose",
    .default = as.character(treatment)
  ))

# Verify it worked:
mosquito_egg_raw_cleaned |>
  distinct(treatment)
# 1 medium_dose
# 2 high_dose  
# 3 low_dose   
# 4 control

# What changed and why it matters:
# It changed because a vector was created containing all the variables which then converted each case into 
# the desired syntax. %in% was need to read the vector. This is important as otherwise when using the variable
# of treatment, there would be 12 categories instead of 4.


