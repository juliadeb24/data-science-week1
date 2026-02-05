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

  

