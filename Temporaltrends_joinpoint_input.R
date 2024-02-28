################## Temporal variation using the PY of observation contributed each year to each practice ##################


# This script outlines the steps taken to prepare the input files for the Joinpoint regression analyses (Figure 1), which modelled temporal trends in test use overall, by test type, gender, age group and deprivation quintile
# The following steps were taken to import, manage, prepare, and export the data 


#### 1. Install and load packages

#### 2.1 Import denominator files
#### 2.2 Format dataframes
#### 2.3 Merge denominator dataframes for each year into one dataframe

#### 3.1 Group the denominator 
#### 3.2 Group by age and gender each year and calculate the sum of registered population

#### 4. Format the denominator dataframe 

#### 5. Format the numerator dataframe

#### 6. Merge the numerator and denominator dataframes
#### 6.1 Calculate crude/unadjusted rate 
#### 6.2 New column containing the 2019 standardised population 
#### 6.3 New column for the standardised adjustment 
#### 6.4 Calculate the adjusted rate 
#### 6.5 Export csv

#### 7. Test rates by age group each year (repeating steps 3-6 but grouping by age group)

#### 8. Test rates by year (repeating steps 3-6 but grouping by year)

#### 9. Test rates by test type each year (repeating steps 3-6 but grouping by test type)

#### 10. Test rates by IMD each year (repeating steps 3-6 but grouping by IMD)

#### 11. Export files into excel to sort for Joinpoint software 


#### -------------------- 1. Install and load packages -------------------- ####


install.packages("readr")
library(readr)
install.packages("doParallel")
library(doParallel)
install.packages("foreach")
library(foreach)
install.packages("writexl")
library(openxlsx)


#### -------------------- 2.1 Import Denominator Files -------------------- ####

PY_2019 <- read_csv("personyears_2019")
PY_2018 <- read_csv("personyears_2018")
PY_2017 <- read_csv("personyears_2017")
PY_2016 <- read_csv("personyears_2016")
PY_2015 <- read_csv("personyears_2015")
PY_2014 <- read_csv("personyears_2014")
PY_2013 <- read_csv("personyears_2013")
PY_2012 <- read_csv("personyears_2012")
PY_2011 <- read_csv("personyears_2011")
PY_2010 <- read_csv("personyears_2010")
PY_2009 <- read_csv("personyears_2009")
PY_2008 <- read_csv("personyears_2008")
PY_2007 <- read_csv("personyears_2007")
PY_2006 <- read_csv("personyears_2006")

#### -------------------- 2.2 Format denominator dataframes -------------------- ####

PY_2019$year <- "2019"
PY_2018$year <- "2018"
PY_2017$year <- "2017"
PY_2016$year <- "2016"
PY_2015$year <- "2015"
PY_2014$year <- "2014"
PY_2013$year <- "2013"
PY_2012$year <- "2012"
PY_2011$year <- "2011"
PY_2010$year <- "2010"
PY_2009$year <- "2009"
PY_2008$year <- "2008"
PY_2007$year <- "2007"
PY_2006$year <- "2006"

colnames(PY_2019)[colnames(PY_2019) == "registered2019"] <- "registered"
colnames(PY_2018)[colnames(PY_2018) == "registered2018"] <- "registered"
colnames(PY_2017)[colnames(PY_2017) == "registered2017"] <- "registered"
colnames(PY_2016)[colnames(PY_2016) == "registered2016"] <- "registered"
colnames(PY_2015)[colnames(PY_2015) == "registered2015"] <- "registered"
colnames(PY_2014)[colnames(PY_2014) == "registered2014"] <- "registered"
colnames(PY_2013)[colnames(PY_2013) == "registered2013"] <- "registered"
colnames(PY_2012)[colnames(PY_2012) == "registered2012"] <- "registered"
colnames(PY_2011)[colnames(PY_2011) == "registered2011"] <- "registered"
colnames(PY_2010)[colnames(PY_2010) == "registered2010"] <- "registered"
colnames(PY_2009)[colnames(PY_2009) == "registered2009"] <- "registered"
colnames(PY_2008)[colnames(PY_2008) == "registered2008"] <- "registered"
colnames(PY_2007)[colnames(PY_2007) == "registered2007"] <- "registered"
colnames(PY_2006)[colnames(PY_2006) == "registered2006"] <- "registered"


#### -------------------- 2.3 Merge the person-years dataframes for each year into one dataframe -------------------- ####
dataframes <- list(PY_2019, PY_2018, PY_2017, PY_2016, PY_2015, PY_2014, PY_2013, PY_2012, PY_2011, PY_2010, PY_2009, PY_2008, PY_2007, PY_2006)  # List of your data frames
PY_by_year <- bind_rows(dataframes)

#### -------------------- 3.1 Grouping the denominator -------------------- ####

# Function to categorize age into age groups
categorize_age <- function(age) {
  case_when(
    age == 0 ~ "0",
    age >= 1 & age <= 5 ~ "1-5",
    age >= 6 & age <= 10 ~ "6-10",
    age >= 11 & age <= 15 ~ "11-15",
    TRUE ~ "Unknown"  # Handle other age values as "Unknown"
  )
}

#### -------------------- 3.2 Grouping the denominator by age and gender each year to calculate the sum of the registered population -------------------- ####

# Group by year, age group, and gender
PY_by_year <- PY_by_year %>%
  mutate(age_group = categorize_age(age),
         gender_group = gender)

# Group by year, age group, and gender, and calculate the sum of registered population
PY_by_year <- PY_by_year %>%
  group_by(year, age_group, gender_group) %>%
  summarize(total_registered = sum(registered))

#### -------------------- 4.1 Format the denominator dataframe -------------------- ####

# Remove rows where gender = I
PY_by_year <- PY_by_year %>%
  filter(gender_group != "I")

# recode PY_by_year table so that f - female and m - male
PY_by_year <- PY_by_year %>%
  mutate(gender_group = recode(gender_group, "F" = "female", "M" = "male"))

### Recode year as test_year
colnames(PY_by_year)[colnames(PY_by_year) == "year"] <- "test_year"



#### -------------------- 5. Format the numerator dataframe -------------------- ####

#Import numerator file (master tests, this was derived from the script for Data cleaning/management and Table 1)
master_tests_agegroups <- read.csv(master_tests_agegroups)

# Define the age groups
age_groups <- c("0", "1-5", "6-10", "11-15")

# Create a new column in master_tests for age groups (Ignore this step if already done previously)
# master_tests_agegroups <- master_tests_2007_precovid %>%
#  mutate(age_group = cut(age, breaks = c(0, 1, 5, 10, 16), labels = age_groups, right = FALSE))

# Calculate the number of panels by grouping the tests by year, age group and gender
summary_table <- master_tests_agegroups %>%
  group_by(test_year, age_group, gender) %>%
  summarise(total_panels = n())

# Filter out gender = I (intersex),  very low numbers
summary_table <- summary_table %>%
  filter(gender != "I")

# recode summary table so that f - female and m - male
summary_table <- summary_table %>%
  mutate(gender = recode(gender, "F" = "female", "M" = "male"))

# Change PY_by_year to gender rather than gender_group
colnames(PY_by_year)[colnames(PY_by_year) == "gender_group"] <- "gender"

#### -------------------- 6. Merge the numerator and denominator dataframe -------------------- ####

# Check the test_year is the same structure in both dataframes
summary_table$test_year <- as.factor(summary_table$test_year)

# Merge using leftjoin
merged_data_agegroup_realPY <- left_join(PY_by_year, summary_table, by = c("test_year", "age_group", "gender"))

#### -------------------- 6.1 Divide tests/population for each row to get a crude/unadjusted rate (tests/1000-child years) -------------------- ####
 
merged_data_agegroup_realPY_rate <- merged_data_agegroup_realPY %>%
  mutate(test_rate = (total_panels / total_registered)*1000)

#### -------------------- 6.2 Create new column containing the standardised population (2019) -------------------- ####

# Make a new standardised population containing the PY population of 2019 for each row 
merged_data_agegroup_realPY_rate <- merged_data_agegroup_realPY_rate %>%
  mutate(std_population = case_when(
    gender == "female" & age_group == "0" ~ 29624.22,
    gender == "female" & age_group == "1-5" ~ 372246.23,
    gender == "female" & age_group == "6-10" ~ 395224.11,
    gender == "female" & age_group == "11-15" ~ 378345.3,
    gender == "male" & age_group == "0" ~ 31196.53,
    gender == "male" & age_group == "1-5" ~ 389884.4,
    gender == "male" & age_group == "6-10" ~ 414292.07,
    gender == "male" & age_group == "11-15" ~ 395176.91,
  ))

merged_data_agegroup_realPY_rate

##### -------------------- 6.3 Make a new column called std_adjustment to adjust for the proportion of each age group in the gender -------------------- ####
 
merged_data_agegroup_realPY_rate <- merged_data_agegroup_realPY_rate %>%
  mutate(std_adjustment = case_when(
    gender == "female" & age_group == "0" ~ 0.025202668,
    gender == "female" & age_group == "1-5" ~ 0.316686751,
    gender == "female" & age_group == "6-10" ~ 0.336235075,
    gender == "female" & age_group == "11-15" ~ 0.321875506,
    gender == "male" & age_group == "0" ~ 0.025351698,
    gender == "male" & age_group == "1-5" ~ 0.316837535,
    gender == "male" & age_group == "6-10" ~ 0.336672301,
    gender == "male" & age_group == "11-15" ~ 0.321138466,
  ))

##### -------------------- 6.4 Adjusted rate -------------------- ####

# Now I want to make a new column called adjusted_rate which equals the test_rate * std_adjustment

merged_data_agegroup_realPY_rate <- merged_data_agegroup_realPY_rate %>%
  mutate(adjusted_rate = test_rate * std_adjustment)

merged_data_agegroup_realPY_rate


##### --------------------  6.5 Export csv--------------------####

write.csv(merged_data_agegroup_realPY_rate, file = "rates_age_sex_year_realPY.csv")



##### --------------------  7. Test rates by age group each year--------------------####

#Calculate the number of panels by age group by grouping the master_tests_agegroups dataframe (Numerator)
summary_table_agegroup <- master_tests_agegroups %>%
  group_by(test_year, age_group) %>%
  summarise(total_panels = n())

# Group the data by test_year and age_group, and calculate the sum of PY_age_sex_population (Denominator)
summarized_age_sex_realPY <- PY_by_year %>%
  group_by(test_year, age_group) %>%
  summarize(PY_age_sex = sum(total_registered)) %>%
  ungroup()

# Merge the numerator and denominator files (first checking the test_year is the same structure in both dataframes)
summary_table_agegroup$test_year <- as.factor (summary_table_agegroup$test_year)
merged_data_onlyagegroup_realPY <- left_join(summarized_age_sex_realPY, summary_table_agegroup, by = c("test_year", "age_group"))

# Divide tests/population for each row to get a rate (tests/1000-child years)
merged_data_onlyagegroup_realPY_rate <- merged_data_onlyagegroup_realPY %>%
  mutate(test_rate = (total_panels / PY_age_sex)*1000)

# Make a new standardised population containing the population of 2019 for each row 
merged_data_onlyagegroup_realPY_rate <- merged_data_onlyagegroup_realPY_rate %>%
  mutate(std_population = case_when(
    age_group == "0" ~ 60820.75,
    age_group == "1-5" ~ 762130.63,
    age_group == "6-10" ~ 809516.18,
    age_group == "11-15" ~ 773522.21,
  ))

merged_data_onlyagegroup_realPY_rate


# Export this as csv
write.csv(merged_data_onlyagegroup_realPY_rate, file = "rates_age_year_realPY.csv")


##### --------------------  8. Test rates by year--------------------####

summary_table_overall <- merged_data_agegroup_realPY %>%
  group_by(test_year) %>%
  summarize(total_panels = sum(total_panels),
            total_registered = sum(total_registered))

merged_data_overall_realPY_rate <- summary_table_overall %>%
  mutate(test_rate = (total_panels / total_registered)*1000)

# Make a new standardised population containing the population of 2019 for each row 
merged_data_overall_realPY_rate <- merged_data_overall_realPY_rate %>%
  mutate(std_population = 2405989.77
)

merged_data_overall_realPY_rate

# Export this as csv

write.csv(merged_data_overall_realPY_rate, file = "rates_year_realPY.csv")

##### --------------------  9. Test rates by test type each year--------------------####

#Calculate the number of each test type by year and age group by grouping the master_tests_agegroups dataframe.
summary_table_test_type <- master_tests_agegroups %>%
  group_by(test_year, test_type, age_group) %>%
  summarise(total_panels = n())

# Merge numerator and denominator dataframes
summary_table_test_type$test_year <- as.factor(summary_table_test_type$test_year)
merged_data_test_type_realPY <- left_join(PY_by_year, summary_table_test_type, by = c("test_year", "age_group"))

# Group the merged_data_test_type by test_year, test_type - NB need to divide panels by 2 as it double counts for each gender otherwise
merged_data_test_type_realPY <- merged_data_test_type_realPY %>%
  group_by(test_year, test_type, age_group) %>%
  summarise(
    total_panels = sum(total_panels)/2,  # Divide total_panels by 2
    total_registered = sum(total_registered)) %>%
  select(age_group, test_year, test_type, total_registered, total_panels) %>%
  distinct()  # Remove duplicate rows if any


# Divide tests/population for each row to get a crude rate (tests/1000-child years)
merged_data_test_type_realPY_rate <- merged_data_test_type_realPY %>%
  mutate(test_rate = (total_panels / total_registered)*1000)

# Make a new standardised population containing the 2019 population
merged_data_test_type_realPY_rate <- merged_data_test_type_realPY_rate %>%
  mutate(std_population = case_when(
    age_group == "0" ~ 60820.75,
    age_group == "1-5" ~ 762130.63,
    age_group == "6-10" ~ 809516.18,
    age_group == "11-15" ~ 773522.21,
  ))

merged_data_test_type_realPY_rate

# Export this as csv
write.csv(merged_data_test_type_realPY_rate, file = "rates_test_type_year_realPY.csv")

##### --------------------  10. Test rates by IMD each year--------------------####

# Import IMD_practice (if not already done)
library(readr)
imd_practice <- read_csv("imd_practice.csv")
View(imd_practice)

#Merge IMD_practice with master dataframe
summary_table_IMD <- left_join(imd_practice, master_tests_agegroups, by = "pracid")

# Remove unnecessary columns 
summary_table_IMD <- summary_table_IMD[, -c(1, 4)]

#Calculate the number of panels by grouping the summary_table_IMD dataframe.
summary_table_IMD <- summary_table_IMD %>%
  group_by(test_year, IMD, age_group ) %>%
  summarise(total_panels = n())

# Then group the IMD into quintiles
summary_table_IMD$imd_quintile <- cut(summary_table_IMD$IMD, 
                                      breaks = c(0, 2, 4, 6, 8, 10),  # Define the quintile breaks
                                      labels = c("1-2", "3-4", "5-6", "7-8", "9-10"),
                                      include.lowest = TRUE)

# Then, aggregate the data to calculate total_panels for each combination of variables (Numerator)
summary_table_IMD <- summary_table_IMD %>%
  group_by(test_year, age_group, imd_quintile) %>%
  summarize(total_panels = sum(total_panels))


# Merge numerator with denominator dataframes (ensuring all variables to be merged are the same structure)
summary_table_IMD$test_year <- as.factor(summary_table_IMD$test_year)
merged_data_IMD <- left_join(PY_by_year, summary_table_IMD, by = c("test_year", "age_group"))


# Divide the total panels by 2 (because it is combining males and females)
# Group the merged_data_IMD by test_year, test_type
merged_data_IMD <- merged_data_IMD %>%
  group_by(test_year, imd_quintile, age_group) %>%
  summarise(
    total_panels = sum(total_panels)/2,  # Divide total_panels by 2
    total_registered = sum(total_registered)
  ) %>%
  select(age_group, test_year, imd_quintile, total_registered, total_panels) %>%
  distinct()  # Remove duplicate rows if any


# Divide tests/population for each row to get a crude rate (tests/1000-child years)
merged_data_IMD_realPY_rate <- merged_data_IMD %>%
  mutate(test_rate = (total_panels / total_registered)*1000)

# Make a new standardised population containing the  population of 2019 for each row
merged_data_IMD_realPY_rate <- merged_data_IMD_realPY_rate %>%
  mutate(std_population = case_when(
    age_group == "0" ~ 60820.75,
    age_group == "1-5" ~ 762130.63,
    age_group == "6-10" ~ 809516.18,
    age_group == "11-15" ~ 773522.21,
  ))

merged_data_IMD_realPY_rate

# Export this as csv
write.csv(merged_data_IMD_realPY_rate, file = "rates_imd_year_realPY.csv")

##### --------------------  11. Export saved csv files into Excel for Joinpoint regression analyses --------------------####
#
# Data have to be sorted as per Joinpoint guidelines for the models to run. This was sorted in Excel before importing into Joinpoint Software



