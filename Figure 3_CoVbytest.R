###################################### Introduction #############################################
#
# This script contains the code to create Figure 3 - a dot/bubble graph which shows the CoV for each test and corresponding test type in the year 2019
# It also conveys the test rate (where the size of the bubble corresponds to the frequency of testing in 2019)
# The data comes from other files where I calculated the adjusted and unadjusted CoV for each test. 
# One of the test files will be uploaded to the repository as an example to demonstrate how the data were obtained
#
#
#
# The following steps were undertaken to create the Figure: 
# Install packages
# Step 1. Import files (a manual excel spreadsheet containing each test name, unadjusted and adjusted CoV and confidence intervals, mean rates of test use)
# Step 2. Formatting the data
# Step 3. Map the test type to a specific colour for the graph
# Step 4. Format graph labels
# Step 5. Create test rate categories/bins so that test rates correspond to a specific bubble size
# Step 6. Create the graph 
# Step 7. Print and save graph 
#
#
#### Install and load packages ####
install.packages("ggplot2")
install.packages("stringr")
library(ggplot2)
library(stringr)


#### --------------------------------- Step 1. Import files ---------------------------------####
#
# Import csv file containing CoV calculated from previous calculations
library(readr)
tests_CoV_2019 <- read_excel("tests_cov_2019.xls") 
aapcbytest <- read_excel("real_PY_forgraphs2.xlsx", 
                         sheet = "tests_overallAPC")

#### --------------------------------- Step 2. Format data ---------------------------------####
#
# To make a new column which contains the % (times CoV by 100)
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(CoV_100 = CoV_adj * 100)

# Order the DataFrame by CoV_100 in descending order
tests_CoV_2019 <- tests_CoV_2019 %>%
  arrange(desc(CoV_100))

#### --------------------------------- Step 3. Map test_type to specific colors ---------------------------------####
#
#Map test_type to shapes and colors
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(
    shape = case_when(
      test_type == "Blood" ~ "circle",
      test_type == "Blood" ~ "circle",
      test_type == "Blood" ~ "circle",
      test_type == "Imaging" ~ "circle",
      test_type == "Miscellaneous" ~ "circle",
      TRUE ~ NA_character_
    ),
    color = case_when(
      test_type == "Blood" ~ "deeppink1",
      test_type == "Imaging" ~ "deepskyblue",
      test_type == "Miscellaneous" ~ "gold",
      TRUE ~ NA_character_))



#### --------------------------------- Step 4. Format graph labels  ---------------------------------####

colnames(tests_CoV_2019)[colnames(tests_CoV_2019) == "test"] <- "test_name"

tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "CXR", "Chest X-Ray", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "US_abdo", "Abdominal ultrasound", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "US_renal", "Renal ultrasound", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Specific_IgE", "Allergen Specific IgE", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Faecal_calprotectin", "Calprotectin", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Coeliac", "Coeliac test", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "MRI_head", "MRI brain", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Immunoglobulins", "Immunoglobulins (IgG, IgA, IgM)", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Helicobacter", "Helicobacter test", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "Wound_Skin_MCS", "Wound/Skin MCS", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "US renal", "Renal ultrasound", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "US abdo", "Abdominal ultrasound", test_name))
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_name = ifelse(test_name == "MRI head", "MRI brain", test_name))

# Remove underscores from the "test_name" column
tests_CoV_2019$test_name <- gsub("_", " ", tests_CoV_2019$test_name)


#### --------------------------------- Step 5. Create test rate categories  ---------------------------------####

# Create a new variable 'test_rate_bin' that represents the test rate bins
# Define the test rate bins
# Show aapc bubble chart with test rate corresponding to bubble size 

# Define the test rate bins
test_rate_bins <- c(-1, 25, 50, 75, Inf)

tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(test_rate_bin = cut(tests_CoV_2019$mean_rate_adjusted, breaks = test_rate_bins, labels = c("<25", "25-49", "50-74", ">=75")))

# Create a numeric variable 'size_value' based on 'test_rate_bin'
tests_CoV_2019 <- tests_CoV_2019 %>%
  mutate(size_value = case_when(
    test_rate_bin == "<25" ~ 3,
    test_rate_bin == "25-49" ~ 5,
    test_rate_bin == "50-74" ~ 7,
    test_rate_bin == ">=75" ~ 10,
    TRUE ~ NA_real_
  ))



#### --------------------------------- Step 6. Create the graph  ---------------------------------####

plot <- ggplot(tests_CoV_2019, aes(x = reorder(test_name, CoV_100), y = CoV_100)) +
  geom_hline(yintercept = 0, linewidth = 0.6, color = "gray") +
  geom_segment(aes(xend = reorder(test_name, CoV_100), yend = 0), color = "grey") +
  geom_point(aes(color = test_type, size = size_value, fill = test_type)) +
  scale_shape_identity() +
  scale_size_identity() +  # Use scale_size_identity() to prevent rescaling
  scale_color_manual(
    name = "Test Type",
    values = c("Blood" = "deeppink1", "Imaging" = "deepskyblue", "Miscellaneous" = "gold"),
    guide = guide_legend(title = "Test Type")
  ) +
  scale_fill_manual(
    name = "Test Type",
    values = c("Blood" = "deeppink1", "Imaging" = "deepskyblue", "Miscellaneous" = "gold")
  ) +
  theme_light() +
  theme(
    panel.grid.major.x = element_line(color = "grey",linetype = "dashed", size = 0.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 16)
  ) +
  scale_y_continuous(breaks = seq(0, max(tests_CoV_2019$CoV_100), by = 50)) +
  labs(
    x = "Test",
    y = "Co-efficient of Variation (%)",
    title = "Rank order of between-practice variability of tests in 2019, adjusted for age, gender, and deprivation",
  ) +
  coord_flip()


# Replace underscores with spaces in the x-axis labels
plot <- plot +
  scale_x_discrete(labels = function(x) str_replace_all(x, "_", " "))

#### --------------------------------- Step 7. Print and save the plot  ---------------------------------####

print(plot)

# Saved as 750 x 1000 png file, and then edited using external software to add legend.

