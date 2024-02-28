####---------------------------------------INTRODUCTION---------------------------------------####

# Below is the code to create Figure 2
# Graph for Figure 4 shows the Average Annual Percentage Change (AAPC) of 35 tests  performed in children aged 0-15 in Oxfordshire
# For this graph I want to create a dotplot with confidence intervals listing the tests in order from highest AAPC growth to the lowest
# The data contributing to this dotplot comes from joinpoint regression analysis that was run on the Joinpoint SEER program 
# The output from Joinpoint was added to a excel spreadsheet including the test name, AAPC, and 95% confidence intervals - called "aapcbytest.xlsx remove 2020"


# The following steps were taken:
# Step 1. Install packages
# Step 2. Import data file from Joipoint output
# Step 3. Clean and prepare data to input into graph
# Step 4. Create graph 
# Step 5. Save graph


# Below this code I have also included the code to create Appendix Figure 1 and 2 (showing AAPC of each test stratified by 1) gender and age and 2) IMD)
# Step 6. Appendix Figure 1 (AAPC of each test by gender and age group)
# Step 7. Appendix Figure 2 (AAPC of each test by IMD category)

#### ----------------------------------Step 1. Install packages-------------------------------------------####


install.packages("dplyr")
install.packages("tidyverse")
install.packages("collapse")
install.packages("janitor")
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(collapse)
library(janitor)

install.packages('hrbrthemes')
install.packages('viridis')
install.packages('ggthemes')
install.packages('gtable')  
install.packages('grid')
install.packages('ggnewscale')
install.packages('geomtext')
install.packages('shadowtext')
install.packages("scales")
install.packages("patchwork")

library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(scales)
library(patchwork)

##########  1. Figure for overall aapc #############
############ 2. Figure for aapc by gender and age ##########

#### ----------------------------------Step 2. Import files-------------------------------------------####


library(readxl)
aapcbytest <- read_excel("real_PY_forgraphs_2007.xlsx", 
                         sheet = "tests_overallAPC")
View(aapcbytest)

#### ----------------------------------Step 3. Clean and prepare data-------------------------------------------####

# Change structure of aapc to numeric variable
aapcbytest$aapc <- as.numeric(aapcbytest$aapc)

# Arrange and transform data
aapc_by_test_graph <- aapcbytest %>%
  arrange(desc(aapc)) %>%
  mutate(test_name = ifelse(test_name == "US abdo", "Abdominal ultrasound",
                            ifelse(test_name == "US Renal", "Renal ultrasound",
                                   ifelse(test_name == "CXR", "Chest X-ray", test_name)))) %>%
  
# Create the main graph

#### ----------------------------------Step 4. Create dot plot-------------------------------------------####
aapc_by_test_graph <- aapcbytest %>%
  arrange(desc(aapc)) %>%
  mutate(test_name = ifelse(test_name == "US abdo", "Abdominal ultrasound",
                            ifelse(test_name == "US Renal", "Renal ultrasound",
                                   ifelse(test_name == "CRP", "C reactive protein",
                                          ifelse(test_name == "CXR", "Chest X-ray", test_name))))) %>%  # Rename specific test_name values
  ggplot(aes(x=reorder(test_name, aapc), y=aapc, color = test_type)) + 
  geom_hline(yintercept = 0, color="light grey", size = 1) +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), width = 0.5, colour = "black") +  # Draw error bars first
  geom_point(size=3) +  # Draw points (dots) second
  geom_segment(aes(x=reorder(test_name, -aapc), 
                   xend=reorder(test_name, -aapc), 
                   y=min(-10), 
                   yend=max(130)), 
               linetype="dashed", color="white", 
               size=0.1) +   # Draw dashed lines
  labs(title="Temporal change of specific test use for children aged 0-15 years of age in English primary care from 2007 to 2019", 
       subtitle="Average annual percentage change of 35 diagnostic tests") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(x="Test name", y = "Average annual percentage change in test use (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11)) +
  theme(axis.title = element_text(size=12,face="bold")) +
  coord_flip() +
  scale_color_manual(values = c("Blood" = "deeppink1", "Imaging" = "deepskyblue", "Other" = "gold")) +
  scale_y_continuous(breaks = seq(-10, 130, by = 10))  # Specify y-axis breaks



#### ----------------------------------Step 5. Print and save graph-------------------------------------------####
aapc_by_test_graph

# Saved as AAPC_test.jpg. Resolution 1300 x 750. I then edited so that there is a break between the two plots. 

#### ----------------------------------Step 6. Create same graph for Appendix Figure 1 (AAPC of each test by gender and age)-------------------------------------------####


# Import sheet "tests_aapc" in real_PY_forgraphs2.xlsx
aapc_gender_age_test <- read_excel("real_PY_forgraphs_2007.xlsx", 
                         sheet = "tests_age_gender_aapc")

#Change gender and age groups to factor variable
aapc_gender_age_test$gender <- as.factor(aapc_gender_age_test$gender)
aapc_gender_age_test$age_group <- factor(aapc_gender_age_test$age_group, levels = c("<1", "1-5", "6-10", "11-15"))
aapc_gender_age_test$overall <- as.numeric(aapc_gender_age_test$overall)

### The following observations were removed because there were fewer than 100 tests in every year from 2007-2019:
### 0 years - HbA1c, Monospot, spirometry, FeNO
### 1-5 years - FeNO
### 6-10 years - FeNO
### The following observations were removed because there were fewer than 1000 tests overall during the study period:
### 0 years - Helicobacter, Calprotectin

# Filter the data frame to remove the specified observations

aapc_gender_age_test <- aapc_gender_age_test %>%
  filter(!(age_group == "<1" & test_name %in% c("HbA1c", "Monospot", "Spirometry", "FeNO", "Helicobacter", "Calprotectin")))
aapc_gender_age_test <- aapc_gender_age_test %>%
  filter(!(age_group == "1-5" & test_name == "FeNO"))
aapc_gender_age_test <- aapc_gender_age_test %>%
  filter(!(age_group == "6-10" & test_name == "FeNO"))

#### --- Make graph: AAPC of each test by gender and age --- ###

aapc_by_test_facet_graph <- aapc_gender_age_test %>%
  arrange(desc(overall)) %>%
  mutate(test_name = ifelse(test_name == "US abdo", "Abdominal ultrasound",
                            ifelse(test_name == "US Renal", "Renal ultrasound",
                                   ifelse(test_name == "CRP", "C reactive protein",
                                          ifelse(test_name == "MRI head", "MRI brain",
                                                 ifelse(test_name == "CXR", "Chest X-ray", test_name)))))) %>%  # Rename specific test_name values
  ggplot(aes(x=reorder(test_name, overall), y=overall, color = test_type)) + 
  geom_hline(yintercept = 0, color="light grey", size = 1) +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), width = 0.5, colour = "black") +  # Draw error bars first
  geom_point(size=3) +  # Draw points (dots) second
  geom_segment(aes(x=reorder(test_name, -overall), 
                   xend=reorder(test_name, -overall), 
                   y=min(-20), 
                   yend=max(40)), 
               linetype="dashed", color="white", 
               size=0.1) +   # Draw dashed lines
  labs(title="Temporal change of specific test use for children aged 0-15 years of age in English primary care from 2007 to 2019", 
       subtitle="Average annual percentage change of 33 diagnostic tests, stratified by gender and age group") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(x="Test name", y = "Average annual percentage change in test use (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11)) +
  theme(axis.title = element_text(size=12,face="bold")) +
  coord_flip() +
  scale_color_manual(values = c("Blood" = "deeppink1", "Imaging" = "deepskyblue", "Other" = "gold")) +
#  scale_y_continuous(breaks = seq(-25, 130, by = 25)) +  # Specify y-axis breaks
  facet_wrap(. ~ gender + age_group, nrow = 2)  # Facet by gender and age group with two rows

aapc_by_test_facet_graph

### Saved as jpg 2000 x 1250 pixels

#### ----------------------------------Step 7. Create same graph for Appendix Figure 2 (AAPC of each test by IMD)-------------------------------------------####


# Import file
aapc_byimd_test <- read_excel("real_PY_forgraphs_2007.xlsx", 
                                   sheet = "imd_by_test")

# Format data frame into correct structure
aapc_byimd_test$imd <- as.numeric(aapc_byimd_test$imd)
aapc_byimd_test$aapc <- as.numeric(aapc_byimd_test$aapc)
aapc_byimd_test$lower_limit <- as.numeric(aapc_byimd_test$lower_limit)
aapc_byimd_test$upper_limit <- as.numeric(aapc_byimd_test$upper_limit)

# Remove NAs
aapc_gender_age_test <- na.omit(aapc_gender_age_test)

#### --- Make graph: AAPC of each test by IMD --- ###

aapc_by_test_imd_facet_graph <- aapc_byimd_test %>%
  arrange(desc(aapc)) %>%
  mutate(test_name = ifelse(test_name == "US abdo", "Abdominal ultrasound",
                            ifelse(test_name == "US Renal", "Renal ultrasound",
                                   ifelse(test_name == "CRP", "C reactive protein",
                                          ifelse(test_name == "MRI head", "MRI brain",
                                                 ifelse(test_name == "CXR", "Chest X-ray", test_name)))))) %>%  # Rename specific test_name values
  ggplot(aes(x=reorder(test_name, aapc), y=aapc, color = test_type)) + 
  geom_hline(yintercept = 0, color="light grey", size = 1) +
  geom_errorbar(aes(ymin = lower_limit, ymax = upper_limit), width = 0.5, colour = "black") +  # Draw error bars first
  geom_point(size=3) +  # Draw points (dots) second
  geom_segment(aes(x=reorder(test_name, -aapc), 
                   xend=reorder(test_name, -aapc), 
                   y=min(-25), 
                   yend=max(125)), 
               linetype="dashed", color="white", 
               size=0.1) +   # Draw dashed lines
  labs(title="Temporal change of specific test use for children aged 0-15 years of age in English primary care from 2007 to 2019 by IMD quintile", 
       subtitle="Average annual percentage change in diagnostic tests") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()) + 
  labs(x="Test name", y = "Average annual percentage change in test use (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11)) +
  theme(axis.title = element_text(size=12,face="bold")) +
  coord_flip() +
  scale_color_manual(values = c("Blood" = "deeppink1", "Imaging" = "deepskyblue", "Other" = "gold")) +
  scale_y_continuous(breaks = seq(-25, 125, by = 25)) +  # Specify y-axis breaks
  facet_wrap(. ~ imd, nrow = 1)  # Facet by gender and age group with two rows

# Print and save graph as 1500 x 1000 resolution jpg file
aapc_by_test_imd_facet_graph
