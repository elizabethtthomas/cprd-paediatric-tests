#####--------------------------------------- Introduction ---------------------------------------#####
#
# This script contains the code to create Figure 4 - a scatterplot which shows the CoV for each test and corresponding test rate in 2019
# It also conveys the whether the test was considered to be higher than average (above the median) rate of test use and higher than average coefficient of variability
# The data comes from other files where I calculated the adjusted and unadjusted CoV for each test. 
# One of the test files will be uploaded to the repository as an example to demonstrate how the data were obtained
#
#
#
# The following steps were undertaken to create the Figure: 
# Install packages
# Step 1. Import files (a manual excel spreadsheet containing each test name, unadjusted and adjusted CoV and confidence intervals, mean rates of test use)
# Step 2. Formatting the data
# Step 3. Calculate median rate and adjusted CoV (Coefficient of Variation)
# Step 4. Calculate ratio of test rate to CoV
# Step 5. Create a new variable to represent the quadrant based on whether the rate/CoV is either above (high) or below (low) the median i.e., high rate - high variability, high rate - low variability, low rate - high variability, low rate - low variability)
# Step 6. Create the scatterplot 
# Step 7. Print and save graph to export for editing 
#
#
#### Install and load packages ####
install.packages("ggplot2")
install.packages("stringr")
library(ggplot2)
library(stringr)
library(readr)
#
#
#
#### --------------------------------- Step 1. Import files ---------------------------------####

tests_CoV_2019 <- read_csv("tests_CoV_2019.csv") 

#### --------------------------------- Step 2. Format data ---------------------------------####

# To make a new column which contains the % (times CoV by 100)
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(CoV_100 = CoV_adj * 100)

tests_CoV_2019$mean_rate_adjusted<- tests_CoV_2019$mean_rate_adjusted*1000


#### --------------------------------- Step 3. Calculate median for mean_rate and adjusted_CoV ---------------------------------####

median_mean_rate <- median(tests_CoV_2019$mean_rate_adjusted)
median_adjusted_CoV <- median(tests_CoV_2019$CoV_100)

#### --------------------------------- Step 4. Calculate the ratio of test rate to CoV ---------------------------------####

tests_CoV_2019$rate_to_cov_ratio <- tests_CoV_2019$mean_rate_adjusted / tests_CoV_2019$CoV_100


#### --------------------------------- Step 5. Create a variable to represent the quadrant ---------------------------------####
#
tests_CoV_2019$median <- factor(ifelse(
  tests_CoV_2019$mean_rate_adjusted >= median_mean_rate & tests_CoV_2019$CoV_100 >= median_adjusted_CoV, "High test rate - High CoV",
  ifelse(tests_CoV_2019$mean_rate_adjusted < median_mean_rate & tests_CoV_2019$CoV_100 < median_adjusted_CoV, "Low test rate - Low CoV",
         ifelse(tests_CoV_2019$mean_rate_adjusted >= median_mean_rate & tests_CoV_2019$CoV_100 < median_adjusted_CoV, "High test rate - Low CoV",
                ifelse(tests_CoV_2019$mean_rate_adjusted < median_mean_rate & tests_CoV_2019$CoV_100 >= median_adjusted_CoV, "Low test rate - High CoV", "")))))

#### --------------------------------- Step 6. Create a scatter plot with different colors for each quadrant ---------------------------------####

p1_ratio <- ggplot(tests_CoV_2019, aes(x = mean_rate_adjusted, y = CoV_100, fill = rate_to_cov_ratio)) +
  geom_point() +
  theme_light() +
  labs(
    x = "Rate of Test Use (tests/1000 child-years)",
    y = "Coefficient of Variation (%)",
    title = "Rate of test utilisation by children aged 0 to 15 and degree of practice variation in 2019"
  ) +
  geom_vline(xintercept = median_mean_rate, size = 0.8, color = "gray") +
  geom_hline(yintercept = median_adjusted_CoV, size = 0.8, color = "gray") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 24))+
  theme(axis.title = element_text(size = 18))+
  theme(legend.position = "none")

tests_CoV_2019$test_name <- gsub("_", " ", tests_CoV_2019$test_name)

# Add labels with ggrepel
p1_ratio + geom_text_repel(aes(label = test_name), size = 6, point.padding = 0.01, segment.color = "transparent", max.overlaps = 8)

#### --------------------------------- Step 7. Save and export graph ---------------------------------####

# Saved and exported as png file, resolution: 1800 x 1000. I then added quadrant shading and labels to the graph in photoediting software.
