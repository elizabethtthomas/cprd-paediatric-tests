############################################# Introduction ############################################# 

# Below shows the code used to create the figure showing temporal trends in overall test use, stratified by gender, age, test_type, imd and each test type
# Step 1. Install relevant packages
# Step 2. Import excel file from Joinpoint output showing temporal changes in overall test rates (modelled and observed)
# Step 3. Use ggplot package to generate line graph
# Step 4. Save as overall_trend_graph
# Step 5. Repeat steps 2 to 4 for temporal changes by test type, sex, sex+age, and by index of multiple deprivation (IMD) quintile
# Step 6. Combine all 5 graphs into one Figure 

############################################# Step 1. Install packages ############################################# 

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
install.packages("viridis")
install.packages("ggrepel")
install.packages("wesanderson")
install.packages("RColorBrewer")
install.packages("rlang")

library(hrbrthemes)
library(viridis)
library(ggthemes)
library(gtable)
library(gtable)
library(ggnewsscale)
library(ggtext)
library(shadowtext)
library(scales)
library(viridis)
library(ggrepel)
library(wesanderson)
library(RColorBrewer)
library(rlang)



############################################# Step 2. Import data file (output from Joinpoint software) ############################################# 

library(readxl)
overall_trend <- read_excel("real_PY_forgraphs.xlsx", 
                            sheet = "overall")
View(overall_trend)

write.csv(PY_per_practice_2019,"PY_per_practice_2019.csv")

#Rename variable names
names(overall_trend)<- c("test_year", "observed_rate","standard_error", "modelled_rate")

# Change test_year to factor variable 
overall_trend$test_year <- as.numeric(overall_trend$test_year)

#############################################  Step 3. Make ggplot graph ############################################# 

overall_trend_graph <- overall_trend %>%
  ggplot(aes(x=test_year, y=modelled_rate))+
  geom_line(linewidth=0.8)+
  geom_point(aes(y=observed_rate, color = "black"))+
  ggtitle("a. Overall test use") +
  theme_bw()+
  scale_color_manual(values=c("black")) +
  xlab("Year")+
  scale_y_continuous(trans= "log10", limits = c(1,1000), name="Test rate (per 1,000 child-years) log axis")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", linewidth = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", linewidth = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2",linetype="dashed", size = 0.6 ))

############################################# Step 4. Print graph ############################################# 

overall_trend_graph

############################################# Step 5. Repeat Steps 2-4 for test type ############################################# 


################## Step 2a Import file: test type ################## 

library(readxl)
temporal_testtype <- read_excel("real_PY_forgraphs.xlsx", 
                                sheet = "test_type")
temporal_testtype$test_type <- as.factor(temporal_testtype$test_type)

################## Step 3a Make graph: test type ################## 

temporal_testtype_graph <- temporal_testtype %>%
  # mutate(label = if_else(year == max(test_year), as.character(gender), NA_character_))%>%
  ggplot(aes(x=test_year, y=modeled_rate, group=test_type, color=test_type))+
  geom_line(size=0.72)+
  ggtitle("b. Test type") +
  theme_bw()+
  scale_color_manual(values = c("orangered", "steelblue1","olivedrab3"))+
  xlab("Year")+
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans= "log10", limits = c(1,1000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face= "bold"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype="dashed" ))
#geom_label_repel(aes(label = label), fill = NA, label.size = NA, point.padding = unit(2, 'lines'),fontface = "bold", na.rm = TRUE, nudge_x = 45, direction = "y")

################## Step 4a Print graph: test type  ################## 

temporal_testtype_graph

################## Step 2b Import file: gender ################## 

library(readxl)
temporal_sex <- read_excel("real_PY_forgraphs.xlsx", 
                           sheet = "gender")
temporal_sex$gender <- as.factor(temporal_sex$gender)

################## Step 3b Make graph: gender ##################  

temporal_sex_graph <- temporal_sex %>%
  ggplot(aes(x=test_year, y=modeled_rate, group=gender, color=gender))+
  geom_line(size=0.72)+
  ggtitle("c. Gender") +
  theme_bw()+
  scale_color_manual(values = c("violetred", "green4"))+
  xlab("Year")+
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans= "log10", limits = c(1,1000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face= "bold"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype="dashed" ))
#geom_label_repel(aes(label = label), fill = NA, label.size = NA, point.padding = unit(2, 'lines'),fontface = "bold", na.rm = TRUE, nudge_x = 45, direction = "y")

################## Step 4b. Print graph: gender  ################## 

temporal_sex_graph

################## Step 2c Import file: gender and age ################## 

library(readxl)
temporal_age_sex <- read_excel("real_PY_forgraphs.xlsx", 
                               sheet = "gender_age")
temporal_age_sex

# Rename variable names
names(temporal_age_sex)<- c("gender", "age_group", "test_year", "observed_rate", "modelled_rate")

# Change age group to factor variable
temporal_age_sex$age_group <- as.factor(temporal_age_sex$age_group)
temporal_age_sex$age_group <- factor(temporal_age_sex$age_group, levels=c("0", "1-5", "6-10", "11-15"))

str(temporal_age_sex$age_group)

################## Step 3c Make graph: gender and age ################## 
# Combine male and female age groups into one graphs
combined_test_rates <- temporal_age_sex %>%
  ggplot(aes(x = test_year, y = modelled_rate, group = interaction(age_group, gender), color = age_group, linetype = gender)) +
  geom_path(size = 0.72) +
  ggtitle("d.Age Group and Gender") +
  theme_bw() +
  scale_color_manual(values = c("deeppink", "royalblue", "chartreuse4", "chocolate1")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "longdash")) +
  xlab("Year") +
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans = "log10", limits = c(1, 1000), name = "Test rate (per 1,000 child-years) log axis") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(2005, 2019)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#b2b2b2", size = 0.6, linetype = "dashed")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  theme(strip.background = element_rect(
    color = "black", fill = "white", linewidth = 1.5, linetype = "solid")) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title = element_text(size = 11, face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

################## Step 4c Print graph: gender and age ################## 
combined_test_rates

################## Step 2d Import file: IMD ################## 

library(readxl)
temporal_imd <- read_excel("real_PY_forgraphs.xlsx", 
                           sheet = "imd")

# Change IMD variable to factor variable
temporal_imd$imd <- as.factor(temporal_imd$imd)

################## Step 3d Make graph: IMD ################## 

temporal_imd_graph <- temporal_imd %>%
  # mutate(label = if_else(year == max(test_year), as.character(gender), NA_character_))%>%
  ggplot(aes(x=test_year, y=modeled_rate, group=imd, color=imd))+
  geom_line(size=0.72)+
  ggtitle("e. IMD quintile") +
  theme_bw()+
  scale_colour_viridis_d(direction = -1)+
  xlab("Year")+
  ylab("Test rate (tests/1,000 child-years)") +
  scale_y_continuous(trans= "log10", limits = c(1,1000), name="Test rate (per 1,000 child-years) log axis")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.major.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, size = 16, face= "bold"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=11))+
  theme(axis.title = element_text(size=11,face="bold"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line( color = "#b2b2b2", size = 0.6, linetype="dashed" ))
#geom_label_repel(aes(label = label), fill = NA, label.size = NA, point.padding = unit(2, 'lines'),fontface = "bold", na.rm = TRUE, nudge_x = 45, direction = "y")

################## Step 4d Print graph: IMD ################## 

temporal_imd_graph


############################################# Step 6. Combine 5 graphs into 1 Figure ############################################# 
install.packages("gridExtra")
library(gridExtra)

plot1 <- overall_trend_graph 
plot2 <- temporal_testtype_graph
plot3 <- temporal_sex_graph
plot4 <- combined_test_rates
plot5 <- temporal_imd_graph

# Saved as Figure1_CPRD, dimensions 2000 x 800  
Figure1_CPRD <- grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 5)



