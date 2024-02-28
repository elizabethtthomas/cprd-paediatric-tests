
########################################## INTRODUCTION ##########################################

# In this script I outline the code used for the descriptive findings in Table 1
# First I prepared the dataframe, including the age groups and IMD quintile for each test and patient
# Then I calculated the number of tests stratified by test type, gender, age group and IMD 
# Then I calculated the median number of tests per year overall, and by gender and age group


######################## Descriptive statistics (Table 1) #############################

master_tests_precovid <- read.csv("master_tests_precovid.csv")

#### Number of females/males

# Load the dplyr package 
library(dplyr)

# master_tests_2007_precovid <- master_tests_precovid (Optional)

# Filter the dataset to keep only unique patid-gender combinations
unique_patients <- master_tests_2007_precovid %>%
  distinct(patid, gender)

# Count the number of unique patients for each gender category
gender_counts <- unique_patients %>%
  group_by(gender) %>%
  summarize(patient_count = n())

# Print the result
print(gender_counts)

########## Creating a dataframe which includes the age groups #############

# Define the age groups
age_groups <- c("0", "1-5", "6-10", "11-15")

# Create a new column in master_tests for age groups
master_tests_agegroups <- master_tests_2007_precovid %>%
  mutate(age_group = cut(age, breaks = c(0, 1, 5, 10, 16), labels = age_groups, right = FALSE))

########## Creating a dataframe which adds the practice IMD for each person + test #############

### Import the practice IMD dataframe (this is to extract the raw IMD txt file)
practice_imd_22_001998 <- read.delim("~/CPRD_tests_children/RDrive/CPRD_tests_children/practice_imd_22_001998.txt")
View(practice_imd_22_001998)

IMD <- practice_imd_22_001998
IMD$e2019_imd_10 <- as.numeric(IMD$e2019_imd_10)
imd_practice <- IMD[,c(1,2,3)]
imd_practice$e2019_imd_10 <- as.numeric(imd_practice$e2019_imd_10)

# Format the IMD dataframe so that it just contains the pracid and the associated IMD decile 
non_numeric_cols <- sapply(imd_practice, function(x) !is.numeric(x))
imd_practice[non_numeric_cols] <- lapply(imd_practice[non_numeric_cols], as.numeric)
imd_practice <- cbind.data.frame(pracid = imd_practice$pracid, IMD = rowSums(imd_practice[, -1], na.rm = TRUE))

#Save file
write.csv(imd_practice.csv)

# Import IMD_practice 
library(readr)
imd_practice <- read_csv("imd_practice.csv")
View(imd_practice)

# Merge IMD_practice with master dataframe
master_tests_agegroups_imd <- left_join(imd_practice, master_tests_agegroups, by = "pracid")

# Remove unnecessary columns 
master_tests_agegroups_imd <- master_tests_agegroups_imd[, -c(1, 4)]

# change to master_tests_agegroups_imd_2007(optional)
# master_tests_agegroups_imd_2007 <- master_tests_agegroups_imd 

# To calculate the IMD median, Q1 and Q3 (by practice)
median(list_IMD$IMD)
quantile(list_IMD$IMD, 0.25)
quantile(list_IMD$IMD, 0.75)

master_tests_agegroups_imd <- master_tests_agegroups_imd %>%
  mutate(IMD_grouped = as.double(ifelse(
    IMD %in% 1:2, 1,
    ifelse(IMD %in% 3:4, 2,
           ifelse(IMD %in% 5:6, 3,
                  ifelse(IMD %in% 7:8, 4,
                         ifelse(IMD %in% 9:10, 5, NA)
                  )
           )
    )
  )))


######### Calculating the number of tests and stratifying by subgroups #########

# 1. Test type 
# 2. Gender 
# 3. Age group 
# 4. IMD (Index of multiple deprivation quintile)

##### Number of tests by test type (Blood/Imaging/Miscellaneous)

# Count the number of tests in each age group
test_type_counts <- master_tests_precovid_age_groups_2007 %>%
  group_by(test_type) %>%
  summarize(test_count = n())

# Print the result
print(test_type_counts)


##### Number of tests by gender 
# Count the number of tests in each gender
gender_tests_counts <- master_tests_agegroups %>%
  group_by(gender) %>%
  summarize(test_count = n())

# Print the result
print(gender_tests_counts)

##### Next to get the number of tests in each age group
# Count the number of tests in each age group
age_counts <- master_tests_agegroups %>%
  group_by(age_group) %>%
  summarize(test_count = n())

# Print the result
print(age_counts)

##### Next to get the number of tests in each IMD
# Count the number of tests in each IMD
IMD_test_counts <- master_tests_agegroups_imd %>%
  group_by(IMD_grouped) %>%
  summarize(test_count = n())

# Print the result
print(IMD_test_counts)



#### Calculating the median number of tests per patient per year ####

tests_per_person_per_year_summary <- master_tests_agegroups %>%
  group_by(patid, test_year) %>%
  summarize(tests_per_person_per_year = n()) %>%
  ungroup() %>%
  summarize(
    median_tests_per_person_per_year = median(tests_per_person_per_year),
    Q1_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.25),
    Q3_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.75)
  )


# Calculate the median and IQR by gender
tests_by_gender_summary <- master_tests_agegroups %>%
  group_by(gender, patid, test_year) %>%
  summarize(tests_per_person_per_year = n()) %>%
  ungroup() %>%
  group_by(gender) %>%
  summarize(
    median_tests_per_person_per_year = median(tests_per_person_per_year),
    Q1_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.25),
    Q3_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.75)
  )

# Calculate the median and IQR by age group
tests_by_agegroup_summary <- master_tests_agegroups %>%
  group_by(age_group, patid, test_year) %>%
  summarize(tests_per_person_per_year = n()) %>%
  ungroup() %>%
  group_by(age_group) %>%
  summarize(
    median_tests_per_person_per_year = median(tests_per_person_per_year),
    Q1_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.25),
    Q3_tests_per_person_per_year = quantile(tests_per_person_per_year, 0.75)
  )

