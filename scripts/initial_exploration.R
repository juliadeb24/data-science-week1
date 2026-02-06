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
# [Code]

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1 |>
  # YOUR CODE
  
  
  # Verify it worked:
  # [Code]
  
  # What changed and why it matters:
  # [2-3 sentences]
  #


