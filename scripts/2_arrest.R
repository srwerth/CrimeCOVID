#-----------------------------------------------
# CLEAN ARREST DATA
# 1_arrest.R
# 09/28/2020
# Rose Werth, Northwestern University
#
# Cleaning script to prepare Chicago arrest data
# for analysis. 
#-----------------------------------------------

### PACKAGES ###------
library(pacman)
p_load(tidyverse, dplyr, ggplot2, janitor, data.table, lubridate)

### LOAD DATA ###-----

# Arrest Data
path_arrest <- "//Volumes/N3_Lab/restricted_data/arrests/clean_anon/arrests_anon_feb2020.csv"
input_arrest <- read_csv(path_arrest)
arrest <- input_arrest 
head(arrest)
names(arrest)

# Homicide Data
path_violent <- "//Volumes/N3_Lab/restricted_data/violent_summ/violent_summ_feb2020.csv"
input_violent <- read_csv(path_violent)
violent <- input_violent

### CLEAN DATA ###------

#' Variables to create: 
#' Count of overall crime per week in 2019 and 2020 (arrest)
#' Count of crimes against property (arrest) 
#' Count of homicides/shootings per week in 2019 and 2020 (homicide/nonfat)

# Limit arrest data to 2019 and 2020 and create week variable
arrest <- arrest %>% 
  mutate(date = ymd(arrest_date)) %>% 
  filter(date >= "2019-01-01" & date <= "2020-12-31") %>% 
  mutate(week = epiweek(date))
# filters from 3751194 to 105,118 obvs
range(arrest$year) # Check
range(arrest$week) # Check

# Create count of crime per week variable
crimecounts <- arrest %>% 
  group_by(year, week) %>% 
  count() %>% 
  rename(arrest_wk = n)
head(crimecounts)

# Limit homicide/shooting data to 2019 and 2020 and create week variable 
violent <- violent %>% 
  filter(date_occ >= "2019-01-01" & date_occ <= "2020-12-31") %>% 
  mutate(week = epiweek(date_occ))
# filters from 27425 to 2395 obvs
range(violent$year) # Check
range(violent$week) # Check

# Create count of homicide/shooting per week variable
addviol <- violent %>% 
  group_by(year, week) %>% 
  count() %>% 
  rename(violence_wk = n)
head(addviol)

# Merge homicide/shooting count onto crime_counts
crimecounts <- crimecounts %>% 
  left_join(addviol, by = c("year", "week"))

head(crimecounts) # Check
head(addviol) # Check

# Create count of property crimes 
#' Property crimes = following FBI codes
#' 03 - Robbery
#' 05 - Burglary 
#' 06 - Larceny
#' 07 - Motor Vehicle Theft
#' 09 - Arson
#' 10 - Forgery/Counterfeiting
#' 11 - Fraud
#' 12 - Embezzlement
#' 13 - Stolen Property
#' 14 - Vandalism
# Fixing the FBI_Code Issues
arrest <- arrest %>% 
  mutate(fbi_code = recode(fbi_code, "2" = "02",
                           "3" = "03",
                           "5" = "05", 
                           "6" = "06",
                           "7" = "07", 
                           "9" = "09"))
# Creating count
addprop <- arrest %>% 
  filter(fbi_code == "03" | fbi_code == "05" | fbi_code == "06" | fbi_code == "07" |
           fbi_code == "09" | fbi_code == "10" | fbi_code == "11" | 
           fbi_code == "12" | fbi_code == "13" | fbi_code == "14") %>% 
  group_by(year, week) %>%
  count() %>% 
  rename(property_wk = n)
head(addprop)  

# Merge homicide/shooting count onto crime_counts
crimecounts <- crimecounts %>% 
  left_join(addprop, by = c("year", "week"))

head(crimecounts) # Check
head(addprop) # Check

### CLEAN UP ENVIRONMENT ###
rm(arrest, violent, addprop, addviol, input_arrest, input_violent, path_arrest, path_violent)
crimecounts

### DESCRIPTIVE ###
#' make some line graphs for 2019 and 2020 layered on each other 
