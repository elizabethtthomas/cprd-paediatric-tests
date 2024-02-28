############################## Urine MCS between practice variation in 2019 ############################## 


#####---------------------------------------Introduction---------------------------------------#####


# This outlines the steps taken to calculate the adjusted CoV for urine_mcs in 2019, this follows on from the coding for Urine MCS
# This code is adapted from code written by Dr Jack O'Sullivan for a paper examining temporal trends and practice variation in testing for adults in primary care, available here: <https://github.com/jackosullivanoxford/Big_data_CPRD>

#### 1. Numerator 
#### 1.1: Import test and restrict to 2019
#### 1.2 Merge with person-years file to generate for each test: patid, age, gender and pracid

#### 2. Denominator
#### 2.1: Import person years file restricted to 2019
#### 2.2: Create denominator stratified into pracid

#### 3. Crude rates
#### 3.1 Import numerator and denominator files
#### 3.2 Create crude rates
#### 3.3 Plot: X-axis Person years, y-axis: rate, dots = 725 practices

#### 4. Standard deviation, mean and CoV
#### 4.1 Calculate std dev
#### 4.2 Calculate mean
#### 4.3 Calculate CoV

#### 5. Adjusted rates
#### 5.1 Import numerator  
#### 5.2 Import denominator and merge with person years
#### 5.3 Calculate proportion of female for each region
#### 5.4 Calculate the proportion of patients >65/age groups in each region
#### 5.5 Add IMD for each practice

#### 6. Poissoning model: Standard deviation, mean and CoV (of Poisson)

#### 7 Plot adjusted and unadjusted rates




##################### 1. Numerator ####################

#### 1.1 Import specific test ####

library(dplyr)
setwd("~/CPRD_tests_children/RDrive/CPRD_tests_children")

# Import csv file containing all Urine MCS tests from 2007 to 2019
library(readr)
urine_mcs_2019 <- read_csv("urine_mcs_2006_2019.csv")


#Restrict to  year 2019
urine_mcs_2019 <- urine_mcs_2019[grepl("2019", urine_mcs_2019$test_year),]

#Sum the number of tests for 2019 by pracid
urine_mcs_2019$test_year <- as.character(urine_mcs_2019$test_year)
urine_mcs_2019 <- urine_mcs_2019 %>% 
  group_by(pracid, patid) %>%
  summarise(urine_mcs_2019= sum(test_year == "2019"))

#Write new file
saveRDS(urine_mcs_2019, file = "urine_mcs_2019.rds")

#### 1.2 Merge with person-years file ####

# Import person_years file
library(readr)
person_years <- read_csv("patientyears_byprac_2019")
person_years <- person_years[,c(2,3)]

#Use Dplyr to generate number per practice in 2019
urine_mcs_per_prac_2019 <- urine_mcs_2019 %>%
  group_by(pracid) %>%
  summarise(No_tests = n())


# Merge urine_mcs_2019 with person_years by practice in 2019
urine_mcs_person_years_2019 <- merge(urine_mcs_per_prac_2019, person_years, by="pracid")


write.csv(urine_mcs_per_prac_2019, file = "UrineMCS_numerator_2019.csv")

##################### 2. Denominator #####################

personyears_2019 <- read_csv("personyears_2019")
prac_personyears_2019 <- read_csv("PY_per_practice_2019")

##################### 3. Crude rates per 1,000 child-years ##################### 

cruderates_urine_mcs_2019 <- merge(prac_personyears_2019, urine_mcs_per_prac_2019, by = "pracid", all = TRUE) #all = TRUE means practices with no test will still be included in dataframe.
cruderates_urine_mcs_2019 <- cruderates_urine_mcs_2019[,-2]
colnames(cruderates_urine_mcs_2019) <- (c("pracid", "PY", "No_tests"))

# Make new column called cruderates
cruderates_urine_mcs_2019$rates <- (cruderates_urine_mcs_2019$No_tests/cruderates_urine_mcs_2019$PY)*1000 #per 1000 person-years
cruderates_urine_mcs_2019$rates1 <- (cruderates_urine_mcs_2019$No_tests/cruderates_urine_mcs_2019$PY) #per 1 person year

#Convert NAs (in the numerator) into 0 (this means no tests)
cruderates_urine_mcs_2019[is.na(cruderates_urine_mcs_2019)] <- 0

# save file
write.csv(cruderates_urine_mcs_2019, file = "cruderates_urine_mcs_2019.csv") 

library(ggplot2)
ggplot() + geom_point(aes(x = cruderates_urine_mcs_2019$PY, y = cruderates_urine_mcs_2019$rates1)) #+ ylim(c(0,5)) #Quick visual inspection, don't saved plot
# on the x axis number of tests per person in 2019, on the y axis number of people in practice (roughly)

##################### 4. Standard deviation, mean and CoV ##################### 

#Standard deviation
sd <- sd(c(cruderates_urine_mcs_2019$rates)) #per 1000

#Mean per 1 person-year
mean <- mean(cruderates_urine_mcs_2019$rates) #per 1000

#Co-efficient of variation
CoV <- sd/mean

##################### 5. Adjusted rates ##################### 


#### 5.1 Merge proportion female with crude rates for Poisson regression #######

personyears_2019 <- read_csv("personyears_2019")
prac_personyears_2019 <- read_csv("PY_per_practice_2019")

#Generate the percentage of person-years that was female
#Calculate total female for each practice
  
Female <- filter(personyears_2019, gender == "F")

library(dplyr)
female_per_practice <- Female %>%
  group_by(pracid) %>%
  summarise(PY_F_2019 = sum(registered2019))

female_1 <- merge(female_per_practice, prac_personyears_2019, by = "pracid") #Don't need 'all = TRUE' because PY_per_practice_2019 has already removed the practices that didn't contribute patients in year 2015

female_1$pro_f <- female_1$PY_F/female_1$PY

female_1 <- female_1[,c(1,2,4,5)]

quantile(female_1$pro_f, 0.25)
quantile(female_1$pro_f, 0.75)


#Save dataframe with proportion female in it for future reference
write.csv(female_1, file = "Proportion_female_per_practice.csv")

### Can start here each time if running repeated tests ###

#Merge Proportion female with crude rates_2019 (for Poisson regression)
cruderates_urine_mcs_f_2019 <- merge(cruderates_urine_mcs_2019, female_1, by = "pracid")
cruderates_urine_mcs_f_2019 <- cruderates_urine_mcs_f_2019[,c(1,2,3,4,5,6,8)]

#Save file
write.csv(cruderates_urine_mcs_f_2019, file = "cruderates_urine_mcs_f_2019.csv")


#####################  5.2 Median ages per practice 2019 ##################### 

personyears_2019 <- read_csv("personyears_2019")
prac_personyears_2019 <- read_csv("PY_per_practice_2019")
cruderates_urine_mcs_f_2019 <- read_csv("cruderates_urine_mcs_f_2019.csv")

#Determine the median age for each practice for year 2019 

#######Median age per practice
median_age_per_practice_2019 <- personyears_2019 %>%
  group_by(pracid) %>%
  summarise(median_age_2019 = median(age))

# Save file 
write.csv(median_age_per_practice_2019, file = "Median_age_per_practice_2019.csv")

### Can start here each time if running repeated tests ###
#Merge with other dataframe with crude rates and proportion of females per practice
cruderates_urine_mcs_f_age_2019 <- merge(cruderates_urine_mcs_f_2019, median_age_per_practice_2019, by = "pracid")

# Save file 
write.csv(cruderates_urine_mcs_f_age_2019, file = "cruderates_urine_mcs_f_age_2019.csv")

#####################  5.3 Add IMD for each practice ##################### 

#Import Practice IMD dataframe 
# No need to do the following if already done before
IMD <- read_csv("practice_imd_22_001998.txt") #Might need to do this manually - then trim the white spaces to delineate the columns
IMD$e2019_imd_10 <- as.numeric(IMD$e2019_imd_10)
list_IMD <- IMD[,c(1,2,3)]
list_IMD$e2019_imd_10 <- as.numeric(list_IMD$e2019_imd_10)


non_numeric_cols <- sapply(list_IMD, function(x) !is.numeric(x))
list_IMD[non_numeric_cols] <- lapply(list_IMD[non_numeric_cols], as.numeric)
list_IMD <- cbind.data.frame(pracid = list_IMD$pracid, IMD = rowSums(list_IMD[, -1], na.rm = TRUE))

list_IMD <- list_IMD %>%
  rename(IMD = list_IMD$e2019_imd_10)

median(list_IMD$IMD)
quantile(list_IMD$IMD, 0.25)
quantile(list_IMD$IMD, 0.75)

# Save IMD as new file
write.csv(list_IMD, file = "IMD_practice.csv")

### Can start here each time if running repeated tests ###

# Merge IMD with overall file (urinemcs_f_age_imd_2019)
urine_mcs_f_age_imd_2019 <- merge(cruderates_urine_mcs_f_age_2019, list_IMD, by = "pracid")

# Save as new csv for model
write.csv(urine_mcs_f_age_imd_2019, file = "urine_mcs_2019_formodel.csv")

##################### 6. Possion model: Standard deviation, mean and CoV ##################### 


#Import data frame
urine_mcs_2019_formodel <- read.csv("urine_mcs_2019_formodel.csv")

mod1_urine_mcs_2019 <- glm(No_tests ~ as.factor(IMD) + pro_f + median_age_2019, 
                          family = poisson, offset = log(PY.x), data = urine_mcs_2019_formodel)%>%
  print()

urine_mcs_2019_formodel$fitted_tests <- fitted(mod1_urine_mcs_2019)
urine_mcs_2019_formodel$fitted_rate1 <- urine_mcs_2019_formodel$fitted_tests/urine_mcs_2019_formodel$PY.x

#Save dataframe with adjusted rates
write.csv(urine_mcs_2019_formodel, file = "urine_mcs2019_with_adjusted_rates.csv")

# Calculating adjusted mean, sd and CoV 
mean_adjusted_urine_mcs2019 <- mean(urine_mcs_2019_formodel$fitted_rate1)

sd_adjusted_urine_mcs2019 <- sd(urine_mcs_2019_formodel$fitted_rate1)

CoV_adjusted_urine_mcs2019 <- sd_adjusted_urine_mcs2019/mean_adjusted_urine_mcs2019

mean_adjusted_urine_mcs2019
# 0.05145684
sd_adjusted_urine_mcs2019
# 0.001953172
CoV_adjusted_urine_mcs2019
# 0.03795748

write.csv(CoV_adjusted_urine_mcs2019, file = "UrineMCS2019_CoV1.csv")


#Manual confidence intervals for adjusted via (http://influentialpoints.com/Training/confidence_interval-of-coefficient_of_variation.htm) #Note that the confidence intervals corrected for bias give the same results
SE_CV_UrineMCS_2019 <- CoV_adjusted_urine_mcs2019/(sqrt(2*nrow(urine_mcs_2019_formodel)))
a <- 0.05

t <- (1-(a/2))

U_CI_CV_adj_urine_mcs2019 <- CoV_adjusted_urine_mcs2019 + t * SE_CV_UrineMCS_2019 

L_CI_CV_adj_urine_mcs2019 <- CoV_adjusted_urine_mcs2019 - t * SE_CV_UrineMCS_2019

U_CI_CV_adj_urine_mcs2019

L_CI_CV_adj_urine_mcs2019

#### Unadjusted mean, SD, CoV
#Unadjusted
mean_unadj_urine_mcs2019 <- mean(urine_mcs_2019_formodel$rates1)

sd_unadj_urine_mcs2019 <- sd(urine_mcs_2019_formodel$rates1)

CoV_unadj_urine_mcs2019 <- sd_unadj_urine_mcs2019/mean_unadj_urine_mcs2019

SE_CV_unadj_urine_mcs2019 <- CoV_unadj_urine_mcs2019/(sqrt(2*nrow(urine_mcs_2019_formodel)))
a <- 0.05

t <- (1-(a/2))

U_CI_CV_unadj_urine_mcs2019 <- CoV_unadj_urine_mcs2019 + t * SE_CV_unadj_urine_mcs2019 

L_CI_CV_unadj_urine_mcs2019 <- CoV_unadj_urine_mcs2019 - t * SE_CV_unadj_urine_mcs2019

U_CI_CV_unadj_urine_mcs2019

L_CI_CV_unadj_urine_mcs2019


# CoVadjusted - CoV unadjusted
CoV_adjusted_urine_mcs2019-CoV_unadj_urine_mcs2019
# -0.4481037

# Print the following values
CoV_adjusted_urine_mcs2019
L_CI_CV_adj_urine_mcs2019
U_CI_CV_adj_urine_mcs2019

CoV_unadj_urine_mcs2019
L_CI_CV_unadj_urine_mcs2019
U_CI_CV_unadj_urine_mcs2019

# Entering the above values into separate spreadsheet called tests_CoV_2019 in project R drive in the following format
# Test	      CoV_adj	  lower_CI_adj	upper_CI_adj	CoV_unadj	  lower_CI_unadj	upper_CI_unadj
# Urine MCS	  0.03795748	0.03727355	   0.03727355	    0.4860612	  0.4773031	      0.4948193

###################### 7. Plot both adjusted and unadjusted rates together ##################### 

library(ggplot2)

ggplot() + geom_point(aes(x=urine_mcs_2019_formodel$PY.x, y = urine_mcs_2019_formodel$rates, colour = "blue"))+
  geom_point(aes(urine_mcs_2019_formodel$PY.x, y = urine_mcs_2019_formodel$fitted_rate1*1000, colour = "red"))+
  ggtitle("Practice-specific Urine MCS request rates for children aged 0 to 15 in 2019")+
  labs(subtitle = "Adjusted for practice population size, sex, age, and deprivation")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  xlab("Child-Years") + ylab("Rates per 1000 child-years")+
  scale_colour_manual(name = "Rates", 
                      labels = c("Unadjusted", "Adjusted"), 
                      values = c("coral2", "steelblue2")) 


#### Save and export graph
# Saved as jpg file with dimensions 900x570
