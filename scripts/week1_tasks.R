#Chater 3 - Strings
penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))

#Trim
#Trim whitespace on either side of a string
str_trim(" Adelie Penguin (Pygoscelis adeliae) ") 
#output = "Adelie Penguin (Pygoscelis adeliae)"
#To trim only from one side:
str_trim("  Adelie Penguin (Pygoscelis adeliae)  ", side = "left")
#output = "Adelie Penguin (Pygoscelis adeliae)  "

#Squish 
#When extra spaces appear in data - use str_squish() to remove leading, trailing, and extra internal white spaces, leaving only single spaces between words.
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")
#output = "Adelie Penguin (Pygoscelis adeliae)"

#Truncate
#to shorten long strings to a specific width — handy when making plots or reports
str_trunc("Adelie Penguin (Pygoscelis adeliae)", width = 18, side = "right")
#output = "Adelie Penguin ..."
#You always want side=right

#Split
#you can split a string into smaller pieces based on a separator
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")
#output = "Adelie"      "Penguin"     "(Pygoscelis" "adeliae)"

#Concatenate
#use this to join pieces of text into one string 
str_c("Adelie", "Penguin", sep = "_")
#output = "Adelie_Penguin"


#Cleaning strings with dplyr
#Look for typos by asking R to produce all of the distinct values in a variable - useful for categorical data
penguins_clean_names |>  
  distinct(sex)

#Conditional Changes with case_when() and if_else()
#to change the text or category labels in your data based on certain conditions
#functions case_when() and if_else() from dplyr 

#case_when()
#when you have multiple conditions to check and different outcomes for each one
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )

#if_else()
#it’s for two-way decisions (something is either true or false)
penguins_clean_names |> 
  mutate(sex = if_else(
    sex == "MALE", "Male", "Female"
  )
  )
#not sure what the purpose of all caps 'MALE' is here 

#Renaming text values with stringr 
# use mutate and case_when - for a statement that conditionally changes 
# the names of the values in a variable
penguins_clean_names |> 
  mutate(species = stringr::word(species, 1)
  ) |> 
  mutate(sex = stringr::str_to_title(sex))

#Convert all species names to uppercase using str_to_upper()
penguins_clean_names |> 
  mutate(species = str_to_upper(species))

#Split columns 
#We could decide we want simpler species names but that we would like to keep the latin name information, but in a separate column
#first, since all species names had been changed using case when - ran penguins_clean_names <- janitor::clean_names(penguins_raw)
#using regex
penguins_clean_names <-
  penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()"
  ) 

#Matching

#Detecting a pattern 
str_detect("Genus specificus", "Genus")
#output = TRUE 
# 3 possible names in species column
penguins_clean_names |> distinct(species)

#Use str_detect() to filter data - filter the species names to only those containing the pattern “papua”
penguins_clean_names |>
  filter(str_detect(species, "papua")) |>
  select(species)

#Remove a pattern 
# remove match for Genus (followed by a whitespace)
str_remove("Genus specificus", pattern = "Genus ")
#output = "specificus"

#Split the species column into a common and latin name column - but it left some ugly brackets - we can use str_remove to strip away those brackets:
penguins_clean_names <-
  penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

#=============================================================================================================================================================

#Chapter 4 - Duplicates

#Check using dplyr 
penguins_clean_names |> 
  filter(duplicated(across(everything())))
  sum() 
#Check using janitor
penguins_clean_names |> 
  get_dupes()

#Making a worksapce to practisse working with duplicates since our data set is duplicate free
penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))

#Rerun duplicates check with the new dataset 
penguins_demo |> 
  filter(duplicated(across(everything())))
sum() 
#output still 0 - no duplicates 

# Keep only unduplicated data with !
penguins_demo |> 
  filter(!duplicated(across(everything())))
#or using distinct 
penguins_demo |> 
  distinct()

#Counting unique entries 
#Using the n_distinct() function from dplyr - count number of distinct values in an R data frame:
penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )

#=============================================================================================================================================================

#Chapter 5 - Missing Data

#Find Missing values
#using summary
summary(penguins_clean_names)
#using skim 
library(skimr)
skimr::skim(penguins_clean_names)
#using vis_miss
library(naniar)
naniar::vis_miss(penguins_clean_names)
#using upset_plot 
naniar::gg_miss_upset(penguins_clean_names)
#Return all rows with a missing value
penguins_clean_names |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15_n_o_oo, delta_13_c_o_oo,comments,
         everything()) # reorder columns
#or specify specific columns NA:
penguins_clean_names |> 
  filter(if_any(culmen_length_mm, is.na))  # reorder columns

#Remove missing values 
#3 main stategies:
#1.Drop all rows with any NA (drop_na()): simple but can remove lots
#2.Drop NAs in specific columns only.
#3.Keep rows, use na.rm = TRUE inside summary functions (least destructive)

#1.drop_na() on everything
#Using drop_na() to remove rows that contain any missing values across all variables
penguins_clean_names |> 
  drop_na()
#Pros - simple, all-in-one solution for datasets where missing values are widespread and problematic
#Cons - may remove entire rows of data, significant data loss if many rows have missing values in non-critical variables

#2.drop_na() on particular variable
#Choose a varible such as body_mass_g
penguins_clean_names |> drop_na(body_mass_g)
#Pros - retain more data since only remove rows with missing values in the columns that matter for current analysis
#Cons - if you overwrite permanently - can't recover dropped rows without reloading data

#3.using arguments inside functions
#more causious approach using the na.rm = TRUE argument
#summary functions (like mean(), median(), or sum()) include this argument, which, when set to TRUE, excludes NA values from the calculation.
#keep missing values in dataset but ignore only when performing calculations
penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )
#Pros - least destructive - handle missing data without removing any rows from the dataset - only exclude NA values during calculations
#Cons - doesn't remove missing data - you need to ensure that they won’t cause issues later in other analyses (e.g., regressions or visualizations)

#To calculate percentage of missing values in each varaible
library(naniar)
miss_var_summary(mosquito_egg_raw)

#=============================================================================================================================================================

#Chapter 6 - Dates 

#There are multiple ways to write same date
library(lubridate)
#using this package, we can reformat dates written in multiple different ways to YYYY-MM-DD

#Reformat
date("2017-10-11T14:02:00")
dmy("11 October 2020")
mdy("10/11/2020")

df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)

df |> 
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

#Extract
#to extract certain elementsof a longer date-time value for summarising, filtering, or plotting data
year("2017-11-28T14:02:00")
month("2017-11-28T14:02:00")
week("2017-11-28T14:02:00")
day("2017-11-28T14:02:00")

#Excel date format 
#Since excel stores dates as serial numbers, dates are often imported into R as the numberic.
#Use janitor package: janitor::excel_numeric_to_date() 
library(janitor)
excel_numeric_to_date(42370)

#For the date_egg variable - use DD-MM-YYYY
penguins_clean_names <- penguins_clean_names |>
  mutate(date_egg = lubridate::dmy(date_egg))

#Calculations wth dates 
penguins_clean_names |> 
  summarise(min_date=min(date_egg),
            max_date=max(date_egg))
#Extract and make new columns from our date column - such as a simple column of the year when each observation was made:
penguins_clean_names <- penguins_clean_names |> 
  mutate(year = lubridate::year(date_egg))

#Filter dates
#Filter datasets to include or exclude data from certain dates or date ranges
penguins_clean_names |>
  filter(date_egg >= ymd("2008-01-01"))

#=============================================================================================================================================================

#Chapter 7 - Check Data Consistency

#Basic range checks 
#Examine the range of your numeric variables and ask: “Are these values biologically possible?”
#Check ranges of all numeric variables at once
penguins_clean_names |> 
  summarise(across(where(is.numeric), 
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))
#Checking range of body mass
penguins_clean_names |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE)
  )

mosquito_egg_raw |> 
  summarise(
    min_mass = min(body_mass_mg, na.rm = TRUE),
    max_mass = max(body_mass_mg, na.rm = TRUE)
  )

#Detecting Impossible values 
#Check for negative values (impossible for mass, length measurements)
penguins_clean_names |> 
  filter(if_any(c(body_mass_g, flipper_length_mm, 
                  culmen_length_mm, culmen_depth_mm), 
                ~ . < 0))
# Check for zero or negative values where zero doesn't make biological sense
penguins_clean_names |> 
  filter(body_mass_g <= 0)

mosquito_egg_raw |> 
  filter(body_mass_mg <= 0)

#Species specific checks
#What’s plausible for one species may be implausible for another:
#Body mass ranges by species
penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )
#We know the typical weight ranges so we could exclude any extremes and outliers
# Find Adelie penguins with Gentoo-sized body mass
penguins_clean_names |> 
  filter(species == "Adelie Penguin (Pygoscelis adeliae)", body_mass_g > 4750)

#Visual inspection
penguins_clean_names |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    x = "",
    y = "Body mass (g)"
  ) +
  theme_minimal()+
  coord_flip()

#Cross variable checks - expected correlations
#Relationships between variables can reveal errors. Body size measurements should correlate positively:
#| label: fig-mass-flipper
#| fig-cap: "Body mass should generally increase with flipper length within species. Points far from the trend may indicate measurement errors."

penguins_clean_names |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
  ) +
  theme_minimal()

# Find penguins with large flippers but low body mass
penguins_clean_names |> 
  filter(flipper_length_mm > 210, body_mass_g < 3500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

# Find penguins with small flippers but high body mass
penguins_clean_names |> 
  filter(flipper_length_mm < 185, body_mass_g > 4500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

#Cross-variable checks: Biological impossibilities
# Males cannot lay eggs
penguins_clean_names |> 
  filter(sex == "MALE", !is.na(date_egg)) |> 
  select(species, sex, date_egg, body_mass_g, island)

#Cross-variable checks: Spatial consistency
# Check which species appear on which islands
penguins_clean_names |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n, values_fill = 0)

#Flagging suspicious values
# Create flags for different types of potential issues
penguins_flagged <- penguins_clean_names |> 
  mutate(
    # Single-variable flags
    flag_impossible = case_when(
      body_mass_g <= 0 ~ "negative_or_zero_mass",
      flipper_length_mm <= 0 ~ "negative_or_zero_flipper",
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_g < 2000 ~ "suspiciously_light",
      body_mass_g > 7000 ~ "suspiciously_heavy",
      TRUE ~ NA_character_
    ),
    
    # Cross-variable flags
    flag_species_size = case_when(
      species == "Adelie" & body_mass_g > 5000 ~ "Adelie_too_heavy",
      species == "Gentoo" & body_mass_g < 4000 ~ "Gentoo_too_light",
      TRUE ~ NA_character_
    ),
    # Any flag present?
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) | 
      !is.na(flag_species_size) 
  )

# Summarize flagged observations
penguins_flagged |> 
  summarise(
    n_impossible = sum(!is.na(flag_impossible)),
    n_implausible = sum(!is.na(flag_implausible)),
    n_species_size = sum(!is.na(flag_species_size)),
    total_flagged = sum(any_flag)
  )

#=============================================================================================================================================================
