######################### Introduction ######################### 
#
# In this script I outline the steps taken to extract, clean and prepare the data for analysis ####
###### Step 1. Install packages
###### Step 2. Extract data 
###### Step 3. Data management 
###### Step 4. Import test codes (pre-prepared)
###### Step 5. Addressing unspecified codes
###### Step 6. Examining and assessing likely duplicates
###### Step 7. Removing duplicate tests in each patient per day
###### Step 8. Restricting date range to pre-covid (ending 2019)
###### Step 9. Exploring the data -  counts for each test 
###### Step 10. Extraction of each test (for test specific analyses)
###### Step 11. Extracting all tests in 2019 (for practice variation analysis)


######################### Step 1. Install packages  #########################

install.packages("DBI")
library(DBI)
install.packages("dplyr")
library(dplyr)


######################### Step 2. Extract data  #########################

# Connect to the workspace on ORCHID
con <- dbConnect(odbc::odbc(), 'CPRDSQL', timeout=10, Database='CPRD_Workspace', uid=rstudioapi::showPrompt(title="Username", message="Username",default=""), pwd=rstudioapi::askForPassword())

#Set working directory 
setwd("~/RDrive/CPRD_20_22_001998")


#July 2023#

df1_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat11_July2023]")
head(df1_new)
nrow(df1_new)

df2_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat20_July2023]")
df3_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat3_July2023]")
df4_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat33_July2023]")
df5_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat34_July2023]")
df6_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat41_July2023]")
df7_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat46_July2023]")
df8_new <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Data_Tests_Cat47_July2023]")

# CPRD demographics
df_demo <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Cohort_Demographics]")

# Denominator data 
prac_2019 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2019_by_Practice]")
prac_2018 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2018_by_Practice]")
prac_2017 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2017_by_Practice]")
prac_2016 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2016_by_Practice]")
prac_2015 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2015_by_Practice]")
prac_2014 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2014_by_Practice]")
prac_2013 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2013_by_Practice]")
prac_2012 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2012_by_Practice]")
prac_2011 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2011_by_Practice]")
prac_2010 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2010_by_Practice]")
prac_2009 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2009_by_Practice]")
prac_2008 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2008_by_Practice]")
prac_2007 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2007_by_Practice]")
prac_2006 <- DBI::dbGetQuery(con, "SELECT ALL * FROM [CPRD_Workspace].[LizzieData].[Registered_in_2006_by_Practice]")


nrow(df_demo)

#save csv
write.csv(prac_2019, "patients_registered_prac_2019")
write.csv(prac_2018, "patients_registered_prac_2018")
write.csv(prac_2017, "patients_registered_prac_2017")
write.csv(prac_2016, "patients_registered_prac_2016")
write.csv(prac_2015, "patients_registered_prac_2015")
write.csv(prac_2014, "patients_registered_prac_2014")
write.csv(prac_2013, "patients_registered_prac_2013")
write.csv(prac_2012, "patients_registered_prac_2012")
write.csv(prac_2011, "patients_registered_prac_2011")
write.csv(prac_2010, "patients_registered_prac_2010")
write.csv(prac_2009, "patients_registered_prac_2009")
write.csv(prac_2008, "patients_registered_prac_2008")
write.csv(prac_2007, "patients_registered_prac_2007")
write.csv(prac_2006, "patients_registered_prac_2006")

########################### Step 3. Data management #########################

# Combine the eight test category dataframes into one using rbind
merged_df_tests <- rbind(df1_new, df2_new, df3_new, df4_new, df5_new, df6_new, df7_new, df8_new)

# Remove any duplicate rows (if present)
merged_df_tests <- distinct(merged_df_tests)

# Sort the merged dataframe by patient ID and observation date
merged_df_tests <- merged_df_tests %>%
  arrange(patid, obsdate)

# Remove any duplicate rows (if present), Updated July 2023
merged_df_tests <- distinct(merged_df_tests, patid, index_date, Leave_Study, obsdate, MedCodeId, Term, .keep_all = TRUE)

# # Sort the merged dataframe by patient ID and observation date,  Updated Jul2023 
merged_df_tests <- merged_df_tests %>%
  arrange(patid, obsdate)

# Next I want to merge merged_df_tests with df_demo
merged_df <- left_join(merged_df_tests, df_demo, by = "patid")

# Optionally, reorder the columns so that the patient characteristics appear after the test data
merged_df <- merged_df %>%
  select(patid, gender, yob, mob, pracid, region, index_date, Leave_Study, obsdate, MedCodeId, Term)

# Remove any duplicate rows (if present)
merged_df <- distinct(merged_df)

### To calculate the age of the patient at the time of the test
install.packages("lubridate")
library(lubridate)

merged_df_new <- merged_df
merged_df_new$age <- NA  # create a new empty column for age

merged_df_new$yob <- as.character(merged_df_new$yob)
merged_df_new$mob <- as.character(merged_df_new$mob)

# If the index_date year and yob is the same, then assume the mob is the same as the index_date
merged_df_new$index_date <- as.Date(merged_df_new$index_date)
str(merged_df_new$index_date)

merged_df_new$mob <- ifelse(year(merged_df_new$index_date) == merged_df_new$yob, month(merged_df_new$index_date), merged_df_new$mob)

merged_df_new$dob <- as.Date(ifelse(merged_df_new$mob == "", paste0(merged_df_new$yob, "-01-01"), paste0(merged_df_new$yob, "-", merged_df_new$mob, "-01")), format = "%Y-%m-%d")
merged_df_new$dob <- as.Date(paste0(merged_df_new$yob, ifelse(is.na(merged_df_new$mob), "-01-01", paste0("-", merged_df_new$mob, "-01"))), format = "%Y-%m-%d")

### Need to check that when this is expanded to full dataset there are some months that are not just January
merged_df_new$age <- floor(as.numeric(difftime(merged_df_new$obsdate, merged_df_new$dob, units = "days")) / 365.25)

# Change the name of variable 
names(merged_df_new)[names(merged_df_new)=="Term"] <- "test_code"


## Check frequency distribution of age
# Install and load the ggplot2 package if you haven't already
install.packages("ggplot2")
library(ggplot2)

# Create a histogram
merged_df_new %>%
  ggplot(merged_df_new, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # You can adjust the binwidth
  labs(x = "Age", y = "Frequency") +
  ggtitle("Age Frequency Distribution")

merged_df_new%>%
  filter(age==0)%>%
  View()


######################### Step 4. Import test codes (pre-prepared) and merge with existing dataframe######################### 

# Import the test codes dataframe, call it test_groups
library(readxl)
test_groups <- read_excel("test_groups_2.xlsx")
View(test_groups)

# Renaming variables
names(test_groups)[names(test_groups)=="Term"] <- "test_code"
names(test_groups)[names(test_groups)=="mapped_to"] <- "panel"

length(unique(test_groups$panel))
length(unique(merged_df_new$test_code))

# merge test codes with merged_df
merged_df_panel <- left_join(merged_df_new, test_groups, by='test_code')
table(merged_df_panel$panel, useNA = "always") # check for completion of merging dataframes 

# Some tests needed to be removed, i.e. MedCode term: Assessment for syringing of ear which is not a diagnostic test
na_panels <- merged_df_panel[is.na(merged_df_panel$panel), ]
# NA panels contain "Assessment for syringing of ear which is not a diagnostic test"
# To remove rows with these observations 

merged_df_panel <- merged_df_panel %>% 
  filter(test_code != "Assessment for syringing of ear")

merged_df_panel$MedCodeId.x <- NULL
merged_df_panel$MedCodeId.y <- NULL    
merged_df_panel$no <- NULL  
merged_df_panel$Observations <- NULL     
merged_df_panel$panel_id <- NULL      

merged_df_panel<- distinct(merged_df_panel)

#change obsdate to date format
merged_df_panel$obsdate = as.Date(merged_df_panel$obsdate)
str(merged_df_panel$obsdate)
merged_df_panel$test_year <- substr (merged_df_panel$obsdate, 1, 4)
str(merged_df_panel$test_year)
merged_df_panel$test_year <- as.factor(merged_df_panel$test_year)
str(merged_df_panel$test_year)

######################### Step 4. Addressing unspecified codes ######################### 

## Concurrent panels - sorting out unspecified/NOS codes  ##
# Find the panels that occur concurrently with the panel "MCS unspecified"
library(tidyverse)


# Filter the dataset to include only "Urine MCS" rows
urine_mcs_data <- merged_df_panel_2 %>%
  filter(panel == "Urine MCS")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_urine_mcs <- urine_mcs_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_urine_mcs, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Urine MCS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()


# This code should correctly count the co-occurrences of panels other than "Urine MCS" on the same day for the same patient and arrange them by frequency. If you still encounter issues or if the results are unexpected, please ensure that your dataset contains the necessary data for these co-occurrences to be identified.


# MCS unspecified 

# Filter the dataset to include only "Urine MCS" rows
mcs_data <- merged_df_panel_2 %>%
  filter(panel == "MCS unspecified")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_mcs_data <- mcs_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_mcs_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "MCS unspecified") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urine MCS, Urinalysis, Sample MCS, sample analysis NOS, and microbiology NOS

## Sample MCS 

# Filter the dataset to include only " MCS unspecified" rows
samplemcs_data <- merged_df_panel_2 %>%
  filter(panel == "Sample MCS")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_samplemcs_data <- samplemcs_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_samplemcs_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Sample MCS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urine MCS, Urinalysis, MCS unspecified, FBC, and microbiology NOS

## Sample analysis NOS 

# Filter the dataset to include only " MCS unspecified" rows
sampleanalysisnos_data <- merged_df_panel_2 %>%
  filter(panel == "Sample analysis NOS")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_sampleanalysis_data <- sampleanalysisnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_sampleanalysis_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Sample analysis NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urine MCS, Urinalysis, MCS unspecified, sample MCS, Microbiology NOS 


## Microbiology NOS

# Filter the dataset to include only " MCS unspecified" rows
microbiologynos_data <- merged_df_panel_2 %>%
  filter(panel == "Microbiology NOS")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_microbiologynos_data <- microbiologynos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_microbiologynos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Microbiology NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urine MCS, Urinalysis, sample MCS, MCS unspecified
#
#
## Scan NOS

# Filter the dataset to include only " Scan NOS" rows
scannos_data <- merged_df_panel_2 %>%
  filter(panel == "Scan NOS")

# Get a list of unique patients and observation dates for "Urine MCS"
scannos_data <- scannos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(scannos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Scan NOS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Diagnostic test NOS, FBC, LFT, Urea and electrolytes, Urine MCS, Urinalysis

## Diagnostic test NOS

# Filter the dataset to include only " Diagnostic test NOS" rows
dxtestnos_data <- merged_df_panel_2 %>%
  filter(panel == "Diagnostic test NOS")

# Get a list of unique patients and observation dates for "Urine MCS"
dxtestnos_data <- dxtestnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(dxtestnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Diagnostic test NOS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with FBC, LFT, Urea and electrolytes, sample MCS, tft

## Adenovirus

# Filter the dataset to include only "Adenovirus" rows
adeno_data <- merged_df_panel_2 %>%
  filter(panel == "Adenovirus test")

# Get a list of unique patients and observation dates for "Urine MCS"
adeno_data <- adeno_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(adeno_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Adenovirus test") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with enteric virus screen, MCS unspecified, Stool OCP, Stool MCS, Sample MCS

# Microscopy parasites
# Filter the dataset to include only "Microscopy parasites" rows
microparasites_data <- merged_df_panel_2 %>%
  filter(panel == "Microscopy parasites")

# Get a list of unique patients and observation dates for "Microscopy parasites"
microparasites_data <- microparasites_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(microparasites_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Microscopy parasites") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Not diagnostic test, Stool OCP, MCS unspecified, stool MCS, Ecoli 0157

# Filter the dataset to include only " Diagnostic test NOS" rows
dxtestnos_data <- merged_df_panel_2 %>%
  filter(panel == "Diagnostic test NOS")

# Get a list of unique patients and observation dates for "Diagnostic test NOS"
dxtestnos_data <- dxtestnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(dxtestnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Diagnostic test NOS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with FBC, LFT, Urea and electrolytes, sample MCS, tft

# Urine test NOS 

# Filter the dataset to include only "Microscopy parasites" rows
urinenos_data <- merged_df_panel_2 %>%
  filter(panel == "Urine test NOS")

# Get a list of unique patients and observation dates for "Microscopy parasites"
urinenos_data <- urinenos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(urinenos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Urine test NOS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urinalysis, Urine MCS, FBC, MCS unspecified, sample MCS
#
#
#
#
# Stool MCS 

# Filter the dataset to include only Stool MCS" rows
stoolmcs_data <- merged_df_panel_2 %>%
  filter(panel == "Stool MCS")

# Get a list of unique patients and observation dates for Stool MCS
stoolmcs_data <- stoolmcs_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(stoolmcs_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel.x != "Stool MCS") %>%
  group_by(panel.x) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urinalysis, Urine MCS, FBC, MCS unspecified, sample MCS

# Viral studies NOS

# Filter the dataset to include only Viral studies NOS rows
viralstudiesnos_data <- merged_df_panel_2 %>%
  filter(panel == "Viral studies NOS")

# Get a list of unique patients and observation dates for Viral studies NOS
viralstudiesnos_data <- viralstudiesnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(viralstudiesnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel!= "Viral studies NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Most commonly occurs with Urinalysis, Urine MCS, FBC, MCS unspecified, sample MCS


# FBC 

# Filter the dataset to include only "FBC" rows
fbc_data <- merged_df_panel_2 %>%
  filter(panel == "FBC")

# Get a list of unique patients and observation dates for "FBC"
unique_fbc_data <- fbc_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_fbc_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "FBC") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

### Haematology NOS 

# Filter the dataset to include only "HaemNOS" rows
haemnos_data <- merged_df_panel %>%
  filter(panel == "Haematology NOS")

# Get a list of unique patients and observation dates for "Urine MCS"
unique_haemnos_data <- haemnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_haemnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  group_by(panel) %>%
  filter(panel != "Haematology NOS") %>%
    summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# Biochemistry NOS 

# Filter the dataset to include only "Biochemistry NOS" rows
biochemnos_data <- merged_df_panel_2 %>%
  filter(panel == "Biochemistry NOS")

# Get a list of unique patients and observation dates for "Biochemistry NOS"
unique_biochemnos_data <- biochemnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_biochemnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Biochemistry NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()


# Peak flow 

# Filter the dataset to include only "Peak flow" rows
peakflow_data <- merged_df_panel_2 %>%
  filter(panel == "Peak flow")

# Get a list of unique patients and observation dates for "Peak flow"
unique_peakflow_data <- peakflow_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_peakflow_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Peak flow") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# US NOS

# Filter the dataset to include only "US NOS" rows
usnos_data <- merged_df_panel_2 %>%
  filter(panel == "US NOS")

# Get a list of unique patients and observation dates for "usnos_data"
unique_usnos_data <- usnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_usnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "US NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# No clear winner 

# Filter the dataset to include only "Urine test NOS" rows
urinetest_data <- merged_df_panel_2 %>%
  filter(panel == "Urine test NOS")

# Get a list of unique patients and observation dates for "unique_urinetest_data"
unique_urinetest_data <- urinetest_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_urinetest_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "Urine test NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

#XR NOS 
# Filter the dataset to include only "XR NOS" rows
xrnos_data <- merged_df_panel_2 %>%
  filter(panel == "XR NOS")

# Get a list of unique patients and observation dates for "unique_urinetest_data"
unique_xrnos_data<- xrnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_xrnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "XR NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# No clear winner

#CT NOS 
# Filter the dataset to include only "CT NOS" rows
ctnos_data <- merged_df_panel_2 %>%
  filter(panel == "CT NOS")

# Get a list of unique patients and observation dates for "unique_ctnos_data"
unique_ctnos_data<- ctnos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_ctnos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "CT NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# No clear winner

#MRI NOS 
# Filter the dataset to include only "MRI NOS" rows
mrinos_data <- merged_df_panel_2 %>%
  filter(panel == "MRI NOS")

# Get a list of unique patients and observation dates for "unique_ctnos_data"
unique_mrinos_data<- mrinos_data %>%
  distinct(patid, obsdate)


# Join the original dataset with itself based on patid and obsdate
co_occurring_panels <- merged_df_panel_2 %>%
  inner_join(unique_mrinos_data, by = c("patid", "obsdate")) 

co_occurring_panels %>%
  filter(panel != "MRI NOS") %>%
  group_by(panel) %>%
  summarise(co_occurrences = n()) %>%
  arrange(desc(co_occurrences))%>%
  View()

# No clear winner

# I want to replace MCS unspecified and Sample MCS with Microbiology NOS 

merged_df_panel_2 <- merged_df_panel %>%
  mutate(panel = ifelse(panel %in% c("MCS unspecified", "Sample MCS"), "Microbiology NOS", panel))

merged_df_panel_2

# Now I have to merge the tests that are likely to be the same test so that I am not double counting for the same patient on the same date

# The order of tests is important (to replace the most common ones first)
# This includes:
# Adenovirus test + enteric virus --> change adenovirus test to enteric virus screen 
# Adenovirus test + respiratory virus screen --> respiratory virus screen
  
# MCS unspecified + urine MCS --> change " to Urine MCS
# MCS unspecified + urinalysis --> change " to Urinalysis
# MCS unspecified + stool MCS --> change MCS unspecified to Stool MCS 
# MCS unspecified + Genital MCS --> change MCS unspecified to Genital MCS 
# MCS unspecified + sputum MCS --> change MCS unspecified to Sputum MCS 
# MCS unspecified + Fungal MCS --> change MCS unspecified to Fungal MCS 
  
# Sample MCS + urine MCS --> CHANGE Sample MCS to urine MCS
# Sample MCS + stool MCS --> change " to stool MCS
# Sample MCS + genital MCS --> change " to Genital MCS
# Sample MCS + wound/skin MCS --> change " to Wound/Skin MCS
# Sample MCS + ENT MCS --> change " to ENT MCS
# Sample MCS + Sputum MCS --> change " to Sputum MCS

# Sample analysis NOS + urine MCS --> Urine MCS
# Sample analysis NOS + urinalysis --> Urinalysis
# Sample analysis NOS + FBC --> FBC
# Sample analysis NOS + Genital MCS --> Genital MCS 

# Microbiology NOS + urine MCS --> urine MCS 
# Microbiology NOS + Enteric virus screen --> Enteric virus screen 
# Microbiology NOS + stool MCS --> stool MCS 
# Microbiology NOS + stool OCP--> stool OCP

# Urine test NOS + urinalysis --> Urinalysis
# Urine test NOS + urine MCS--> Urine MCS
# Urine test NOS + Urine biochemistry--> Urine biochemistry

# Microscopy parasites + stool OCP --> stool OCP 

# FBC + Haematology NOS --> FBC 

# Biochemistry NOS + LFT --> LFT
# Biochemistry NOS + Urea and electrolytes --> Urea and electrolytes
# Biochemistry NOS + Bone profile --> Bone profile
# Biochemistry NOS + TFT --> TFT
# Biochemistry NOS + CRP--> CRP
# Biochemistry NOS + Iron studies --> Iron studies
# Biochemistry NOS + Glucose --> Glucose
# Biochemistry NOS + ESR --> ESR
# Biochemistry NOS + Allergen Specific IgE --> Allergen Specific IgE
# Biochemistry NOS + Lipids --> Lipids 
# Biochemistry NOS + Urine biochemistry  --> Urine biochemistry

########### Step 5. Examining and assessing likely duplicates ########### 

check_data <- merged_df_panel %>%
  group_by(patid, obsdate) %>%
  summarise(both_present = all(c("Adenovirus test", "Enteric virus screen") %in% panel))

# Count the number of groups where both panels are present
count(check_data, both_present)

# Define a function to perform conditional replacements
replace_panels <- function(df, target_panel, replacement_panel, conditional_panel) {
  df %>%
    group_by(patid, obsdate) %>%
    mutate(panel = ifelse(panel == target_panel & conditional_panel %in% panel,
                          replacement_panel, panel)) %>%
    ungroup()
}

# Apply replacements one by one
merged_df_panel_3 <- merged_df_panel_2 %>%
  replace_panels("Microbiology NOS", "Urine MCS", "Urine MCS") %>%
  replace_panels("Microbiology NOS", "Urinalysis", "Urine MCS") %>%
  replace_panels("Microbiology NOS", "Stool MCS", "Stool MCS") %>%
  replace_panels("Microbiology NOS", "Genital MCS", "Genital MCS") %>%
  replace_panels("Microbiology NOS", "Sputum MCS", "Sputum MCS")%>%
  replace_panels("Microbiology NOS", "Fungal MCS", "Fungal MCS")%>%
  replace_panels("Microbiology NOS", "Wound/Skin MCS", "Wound/Skin MCS") %>%
  replace_panels("Microbiology NOS", "ENT MCS", "ENT MCS") %>%
  replace_panels("Microbiology NOS", "Enteric virus screen", "Enteric virus screen")%>%
  replace_panels("Microbiology NOS", "Stool OCP", "Stool OCP")
  

merged_df_panel_4 <- merged_df_panel_3 %>%
  replace_panels("Adenovirus test", "Enteric virus screen", "Enteric virus screen")%>%
  replace_panels("Adenovirus test", "Respiratory virus screen", "Respiratory virus screen")%>%
  replace_panels("Sample analysis NOS", "Urine MCS", "Urine MCS")%>%
  replace_panels("Sample analysis NOS", "Urinalysis", "Urinalysis")



merged_df_panel_5 <- merged_df_panel_4 %>%
  replace_panels("Haematology NOS", "FBC", "FBC") %>%
  replace_panels("Biochemistry NOS", "LFT", "LFT") %>%
  replace_panels("Biochemistry NOS", "Urea and electrolytes", "Urea and electrolytes") %>%
  replace_panels("Biochemistry NOS", "Bone profile", "Bone profile") %>%
  replace_panels("Biochemistry NOS", "TFT", "TFT") %>%
  replace_panels("Biochemistry NOS", "CRP", "CRP") %>%
  replace_panels("Biochemistry NOS", "Iron studies", "Iron studies") %>%
  replace_panels("Biochemistry NOS", "Glucose", "Glucose") %>%
  replace_panels("Biochemistry NOS", "ESR", "ESR") %>%
  replace_panels("Biochemistry NOS", "Allergen Specific IgE", "Allergen Specific IgE") %>%
  replace_panels("Biochemistry NOS", "Lipids", "Lipids") %>%
  replace_panels("Biochemistry NOS", "Urine biochemistry", "Urine biochemistry")
  
  
# Count MCS unspecified
count_mcs_unspecified <- sum(master_tests$panel == "Sample MCS")

# Print the count
print(count_mcs_unspecified)

  
check_data <- merged_df_panel_5 %>%
  filter(panel == "Microbiology NOS")

# Check inidividual patient id

dummy <- subset(master_condensed, master_condensed$patid=="6078841621419")   # examining id 127020018 which had 2 of same test on one day by creating dummy dataframe (blood gas - examined to be correct)
dummy <- dummy[order(dummy$obsdate, dummy$panel),] %>%
  View ()# ordered dummy dataset by date and panel 
  

### Change one of the observations to MRI head 
merged_df_panel_6 <- merged_df_panel_5 %>%
  mutate(panel = case_when(
    grepl("Magnetic resonance imaging of brain normal", test_code) ~ "MRI head",
    TRUE ~ panel
  ))

# Check that it worked

dummy <- subset(merged_df_panel_6, merged_df_panel_6$test_code=="Magnetic resonance imaging of brain normal")   # examining id 127020018 which had 2 of same test on one day by creating dummy dataframe (blood gas - examined to be correct)
dummy <- dummy[order(dummy$obsdate, dummy$panel),] %>%
  View ()# ordered dummy dataset by date and panel 

rm(dummy)

# I want to change the value of "panel"  in the rows where "test_code" = "Serum blood tests" and "panel" = "Urine biochemistry" to "Biochemistry NOS" AS Serum blood tests cannot be mapped to Urine biochemistry and should remain as Biochemistry NOS (as per initial testcodes)
merged_df_panel_7 <- merged_df_panel_6 %>%
  mutate(panel = case_when(
    test_code == "Serum blood tests" & panel == "Urine biochemistry" ~ "Biochemistry NOS",
    TRUE ~ panel
  ))

# Check that it worked

# Check specific rows where test_code is "Serum blood tests" and panel is "Urine biochemistry"
filtered_rows <- merged_df_panel_7 %>%
  filter(test_code == "Serum blood tests" & panel == "Urine biochemistry")

# Print the filtered rows
print(filtered_rows)
# 0 results so it worked 

#
# I need to remove the blood test codes called "Blood oxygen saturation (calculated), Blood oxygen saturation, Capillary oxygen saturation, Oxygen saturation studies and Oxygenation index"

#### Define a vector of observations to be removed
observations_to_remove <- c(
  "Blood oxygen saturation (calculated)",
  "Blood oxygen saturation",
  "Capillary oxygen saturation",
  "Oxygen saturation studies",
  "Oxygenation index"
)

# Remove rows with the specified observations in the "test_code" variable
merged_df_panel_8 <- merged_df_panel_7 %>%
  filter(!(test_code %in% observations_to_remove))

# Check that they were removed 
dummy <- subset(merged_df_panel_8, merged_df_panel_8$test_code=="Blood oxygen saturation")   # examining id 127020018 which had 2 of same test on one day by creating dummy dataframe (blood gas - examined to be correct)
dummy <- dummy[order(dummy$obsdate, dummy$panel),] %>%
  View ()# ordered dummy dataset by date and panel 

# 0 results so it worked


# Test to see if worked, answer should be 0 
subset_df <- merged_df_panel_8 %>%
  group_by(patid, obsdate) %>%
  filter("Microscopy parasites" %in% panel & "stool OCP" %in% panel) %>%
  ungroup()
# it worked
# remove subset_df
rm(subset_df)

################ Step 6. Removing duplicate tests in each patient per day ################ 

#### Make a new variable called no_test_perday then delete rows which are not 1 ####
merged_df_panel_9 <- merged_df_panel_8 %>%
  group_by(patid,obsdate,panel) %>%
  mutate(no_test_perday = 1:n())  

table(merged_df_panel_9$no_test_perday, useNA = "always")   # TEST - check for completeness of new variable (how many of each test were done on each day)


# New data frame with unique no_test_perday (deleting rows which are not 1)
master_condensed <- subset(merged_df_panel_9, merged_df_panel_9$no_test_perday==1)


# Check how many unique tests are in the sample file
length(unique(master_condensed$panel))

################ Step 7. Remove tests that are not actually diagnostic tests ################ 
master_tests <-master_condensed[!grepl('Not diagnostic test', master_condensed$panel),]

# Check how many unique tests are in the sample file
length(unique(master_tests$panel))

saveRDS(master_tests, file = "master.rds")

################ Step 8. Pre-covid tests only ################ 

# Condense to tests pre-covid
master_tests_precovid <-master_tests[!grepl("2020|2021", master_tests$test_year),]

write.csv(master_tests_precovid, file = "master_tests_precovid.csv")

# Later I removed tests in 2006 as the data cut did not capture patients <1 year born in 2005 who had a test in 2006 
master_tests_precovid <-master_tests_precovid[!grepl("2006", master_tests_precovid$test_year),]

write.csv(master_tests_precovid, file = "master_tests_precovid.csv")

################ Step 9. Most common test numbers overall  ################ 

master_tests_precovid %>%
  group_by(panel) %>%
  count() %>%
  View()
 
breakdown <- master_tests_precovid %>%
  filter(panel == "FBC") %>%
  group_by(age, gender) %>%
  summarise(FBC_count = n())

breakdown_urine <- master_tests_precovid %>%
  filter(panel == "Urine MCS") %>%
  group_by(age, gender) %>%
  summarise(urinemcs_count = n())

breakdown_micronos <- master_tests_precovid %>%
  filter(panel == "Microbiology NOS") %>%
  group_by(age) %>%
  summarise(micronos_count = n())

master_tests_precovid %>%
  group_by(test_year) %>%
  count() %>%
  View()

################ Step 10. Extracting each test ################ 

urine_mcs_2006_2019 <- subset(master_tests_precovid, panel == "Urine MCS")
write.csv(urine_mcs_2006_2019, file = "urine_mcs_2006_2019.csv")

urinalysis_2006_2019 <- subset(master_tests_precovid, panel == "Urinalysis")
write.csv(urinalysis_2006_2019, file = "urinalysis_2005_2016.csv")

fbc_2006_2019 <- subset(master_tests_precovid, panel == "FBC")
write.csv(fbc_2006_2019, file = "fbc_2006_2019.csv")

peakflow_2006_2019 <- subset(master_tests_precovid, panel == "Peak flow")
write.csv(peakflow_2006_2019, file = "peakflow_2006_2019.csv")

ue_2006_2019 <- subset(master_tests_precovid, panel == "Urea and electrolytes")
write.csv(ue_2006_2019, file = "ue_2006_2019.csv")

lft_2006_2019 <- subset(master_tests_precovid, panel == "LFT")
write.csv(lft_2006_2019, file = "lft_2006_2019.csv")

tft_2006_2019 <- subset(master_tests_precovid, panel == "TFT")
write.csv(tft_2006_2019, file = "tft_2006_2019.csv")

ironstudies_2006_2019 <- subset(master_tests_precovid, panel == "Iron studies")
write.csv(ironstudies_2006_2019, file = "ironstudies_2006_2019.csv")

boneprofile_2006_2019 <- subset(master_tests_precovid, panel == "Bone profile")
write.csv(boneprofile_2006_2019, file = "boneprofile_2006_2019.csv")

glucose_2006_2019 <- subset(master_tests_precovid, panel == "Glucose")
write.csv(glucose_2006_2019, file = "glucose_2006_2019.csv")

crp_2006_2019 <- subset(master_tests_precovid, panel == "CRP")
write.csv(crp_2006_2019, file = "crp_2006_2019.csv")

esr_2006_2019 <- subset(master_tests_precovid, panel == "ESR")
write.csv(esr_2006_2019, file = "esr_2006_2019.csv")

stoolmcs_2006_2019 <- subset(master_tests_precovid, panel == "Stool MCS")
write.csv(stoolmcs_2006_2019, file = "stoolmcs_2006_2019.csv")

b12_2006_2019 <- subset(master_tests_precovid, panel == "Vitamin B12")
write.csv(b12_2006_2019, file = "b12_2006_2019.csv")

folate_2006_2019 <- subset(master_tests_precovid, panel == "Folate")
write.csv(folate_2006_2019, file = "folate_2006_2019.csv")

vitd_2006_2019 <- subset(master_tests_precovid, panel == "Vitamin D")
write.csv(vitd_2006_2019, file = "vitd_2006_2019.csv")

hba1c_2006_2019 <- subset(master_tests_precovid, panel == "HbA1c")
write.csv(hba1c_2006_2019, file = "hba1c_2006_2019.csv")

entericvirus_2006_2019 <- subset(master_tests_precovid, panel == "Enteric virus screen")
write.csv(entericvirus_2006_2019, file = "entericvirus_2006_2019.csv")

wound_skinmcs_2006_2019 <- subset(master_tests_precovid, panel == "Wound/Skin MCS")
write.csv(wound_skinmcs_2006_2019, file = "wound_skinmcs_2006_2019.csv")

spirometry_2006_2019 <- subset(master_tests_precovid, panel == "Spirometry")
write.csv(spirometry_2006_2019, file = "spirometry_2006_2019.csv")

coeliac_2006_2019 <- subset(master_tests_precovid, panel == "Coeliac test")
write.csv(coeliac_2006_2019, file = "coeliac_2006_2019.csv")

cxr_2006_2019 <- subset(master_tests_precovid, panel == "CXR")
write.csv(cxr_2006_2019, file = "cxr_2006_2019.csv")

ecg_2006_2019 <- subset(master_tests_precovid, panel == "ECG")
write.csv(ecg_2006_2019, file = "ecg_2006_2019.csv")

stoolocp_2006_2019 <- subset(master_tests_precovid, panel == "Stool OCP")
write.csv(stoolocp_2006_2019, file = "stoolocp_2006_2019.csv")

urinebiochem_2006_2019 <- subset(master_tests_precovid, panel == "Urine biochemistry")
write.csv(urinebiochem_2006_2019, file = "urinebiochem_2006_2019.csv")

specIgE_2006_2019 <- subset(master_tests_precovid, panel == "Allergen Specific IgE")
write.csv(specIgE_2006_2019, file = "specIgE_2006_2019.csv")

usabdo_2006_2019 <- subset(master_tests_precovid, panel == "US abdo")
write.csv(usabdo_2006_2019, file = "usabdo_2006_2019.csv")

feno_2006_2019 <- subset(master_tests_precovid, panel == "FeNO")
write.csv(feno_2006_2019, file = "feno_2006_2019.csv")

helicobacter_2006_2019 <- subset(master_tests_precovid, panel == "Helicobacter test")
write.csv(helicobacter_2006_2019, file = "helicobacter_2006_2019.csv")

usrenal_2006_2019 <- subset(master_tests_precovid, panel == "US Renal")
write.csv(usrenal_2006_2019, file = "usrenal_2006_2019.csv")

cthead_2006_2019 <- subset(master_tests_precovid, panel == "CT head")
write.csv(cthead_2006_2019, file = "cthead_2006_2019.csv")

mrihead_2006_2019 <- subset(master_tests_precovid, panel == "MRI head")
write.csv(mrihead_2006_2019, file = "mrihead_2006_2019.csv")

calprotectin_2006_2019 <- subset(master_tests_precovid, panel == "Calprotectin")
write.csv(calprotectin_2006_2019, file = "calprotectin_2006_2019.csv")

hearing_2006_2019 <- subset(master_tests_precovid, panel == "Hearing test") # <------ add this to prac var analysis
write.csv(hearing_2006_2019, file = "hearing_2006_2019.csv")

monospot_2006_2019 <- subset(master_tests_precovid, panel == "Monospot") # <-add this to prac var analysis
write.csv(monospot_2006_2019, file = "monospot_2006_2019.csv")

immunoglobulins_2006_2019 <- subset(master_tests_precovid, panel == "Immunoglobulins (IgG, IgA, IgM)") # <-add this to prac var analysis
write.csv(immunoglobulins_2006_2019, file = "immunoglobulins_2006_2019.csv")


################# Step 11. Preparing dataframe including tests only in 2019 ################ 

master_test_2019 <- master_tests_precovid[grepl('2019', master_tests_precovid$test_year),]

# Sum up the number of tests for 2019
master_test_2019$test_year <- as.character(master_test_2019$test_year)

master_test_2019 <- master_test_2019 %>% 
  group_by(patid, pracid, age, gender) %>%
  summarise(test_2019 = sum(test_year == "2019"))

#Set working directory 
setwd("~/RDrive/CPRD_20_22_001998")

# Write new file
saveRDS(master_test_2019, file = "tests_2019.rds")
