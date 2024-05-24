##
# Author: Gulce Erincik
# Created: 05.24.2024
# 
# Description: The Residential Building Stock in Istanbul - examination using the Building Permits Data from TUIK. 
##

install.packages("readxl")
install.packages("tidyverse")

library("readxl")
library("tidyverse")

# ================================================
# read xlsx files (yapi kullanma izin belgelerine gore veriler/data for building permits)
# ================================================

my_data <- read_excel("BOP1.xlsx")
glimpse(my_data)

# ================================================
# Prepare data
# ================================================

# Clean the rows with years less than 2008
unique(my_data$...2)

my_data_2 <- my_data %>%
  select(...2, ...4, ...5, ...7, ...8, ...9, ...10, ...11, ...12, ...13, ...14) %>% 
  filter(...2 > 2007 & ...7 %in% c("İki ve daha fazla daireli binalar")) %>% 
  rename("Year" = ...2, "City" = ...4, "Municipality" = ...5, "Function" = ...7, "NumberOfStoreys" = ...8, "NumberOfBuildings" = ...9, "NumberOfFlats" = ...10, "Area" = ...11, "ResidentialArea" = ...12, "OtherArea" = ...13, "CommonArea" = ...14)

glimpse(my_data_2)

# Clean also the rows if "BinaSayisi" is "0"
my_data_3 <- subset(my_data_2, NumberOfBuildings != "0")

glimpse(my_data_3)

# Omit NAs
my_data_3 <- na.omit(my_data_3)

# ================================================
# Number of storeys for Istanbul and Maltepe
# ================================================

class(my_data_3$NumberOfBuildings)
my_data_3$NumberOfBuildings <- as.numeric(my_data_3$NumberOfBuildings)

# ================================================Number of Storeys for Maltepe

my_data_Maltepe_NumberOfStoreys <- my_data_3 %>%
  select(Year, Municipality, NumberOfStoreys, NumberOfBuildings) %>% 
  filter(Municipality == "Maltepe")

# Group by NumberOfStoreys and summarize
dt_Maltepe_NumberOfStoreys <- my_data_Maltepe_NumberOfStoreys %>%
  group_by(NumberOfStoreys) %>%
  summarize(Sum_NumberOfBuildings = sum(NumberOfBuildings))

# Print the result
print(dt_Maltepe_NumberOfStoreys)

# ================================================Number of Storeys for Istanbul

my_data_Istanbul_NumberOfStoreys <- my_data_3 %>%
  select(Year, Municipality, NumberOfStoreys, NumberOfBuildings)

# Group by NumberOfStoreys and summarize
dt_Istanbul_NumberOfStoreys <- my_data_Istanbul_NumberOfStoreys %>%
  group_by(NumberOfStoreys) %>%
  summarize(Sum_NumberOfBuildings = sum(NumberOfBuildings))

# Print the result
print(dt_Istanbul_NumberOfStoreys)

# ================================================Visualize Number of Storeys

# Create a bar plot for Maltepe
ggplot(dt_Maltepe_NumberOfStoreys, aes(x = NumberOfStoreys, y = Sum_NumberOfBuildings)) +
  geom_bar(stat = "identity", fill = "#F5C464") +
  labs(title = "Number of Storeys of Multifamily Residential Buildings in Maltepe",
       x = "Number of Storeys per Building",
       y = "Number of Multifamily Residential Buildings") +
  theme_light() +
  theme(
    plot.title = element_text(family = "Helvetica", size = 11, face = "bold", color = "black"), # Adjust title font
    axis.title.x = element_text(family = "Helvetica", size = 11, color = "#545759", vjust = +0.2, hjust = -0.01), # Adjust x-axis label font
    axis.title.y = element_text(family = "Helvetica", size = 11, color = "#545759", vjust = +2, hjust = -0.04), # Adjust y-axis label font
    axis.text.x = element_text(size = 11, color = "black"), # Adjust x-axis tick label font size
    axis.text.y = element_text(size = 11, color = "black") # Adjust y-axis tick label font size
  )

# Create a bar plot for Istanbul
ggplot(dt_Istanbul_NumberOfStoreys, aes(x = NumberOfStoreys, y = Sum_NumberOfBuildings)) +
  geom_bar(stat = "identity", fill = "#F5C464") +
  labs(title = "Number of Storeys of Multifamily Residential Buildings in Istanbul",
       x = "Number of Storeys per Building",
       y = "Number of Multifamily Residential Buildings") +
  theme_light() +
  theme(
    plot.title = element_text(family = "Helvetica", size = 11, face = "bold", color = "black"), # Adjust title font
    axis.title.x = element_text(family = "Helvetica", size = 11, color = "#545759", vjust = +0.2, hjust = -0.01), # Adjust x-axis label font
    axis.title.y = element_text(family = "Helvetica", size = 11, color = "#545759", vjust = +2, hjust = -0.04), # Adjust y-axis label font
    axis.text.x = element_text(size = 11, color = "black"), # Adjust x-axis tick label font size
    axis.text.y = element_text(size = 11, color = "black") # Adjust y-axis tick label font size
  )

# ================================================
# Regulating the classes of data
# ================================================

class(my_data_3$NumberOfFlats)
my_data_3$NumberOfFlats <- as.numeric(my_data_3$NumberOfFlats)
my_data_3$ResidentialArea <- as.numeric(my_data_3$ResidentialArea)
my_data_3$NumberOfStoreys <- as.numeric(my_data_3$NumberOfStoreys)
my_data_3$Area <- as.numeric(my_data_3$Area)
my_data_3_Manipulated <- my_data_3

# ================================================
# Prepare data for the box plots: examining the plan configurations
# ================================================

# Filter for 6-storey buildings
dt_six_storey_buildings <- my_data_3_Manipulated %>%
  filter(NumberOfStoreys == 6)

# Select only the necessary columns 
dt_6_Story_Areas <- dt_six_storey_buildings %>%
  select(Year, Municipality, Function, NumberOfStoreys, NumberOfBuildings, NumberOfFlats, ResidentialArea, CommonArea)

class(dt_6_Story_Areas$CommonArea)
dt_6_Story_Areas$CommonArea <- as.numeric(dt_6_Story_Areas$CommonArea)

# Calculate the total area by summing the ResidentialArea and CommonArea
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(TotalArea = ResidentialArea + CommonArea)

# Calculate the total area per storey
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(TotalAreaPerStorey = TotalArea / NumberOfStoreys)

# Calculate the core area per storey
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(TotalAreaPerStorey = CommonArea / NumberOfStoreys)

# Calculate MeterSquare per Flat
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(MetersquarePerFlat = ResidentialArea / NumberOfFlats)

# Calculate Number of flats per storey
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(FlatsPerStorey = NumberOfFlats / NumberOfStoreys)

# Calculate Common area per storey
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(CommonAreaPerBuilding = CommonArea / NumberOfBuildings)
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(CommonAreaPerStorey = CommonAreaPerBuilding / NumberOfStoreys)

# Round values
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(MetersquarePerFlat  = round(MetersquarePerFlat))
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(FlatsPerStorey  = round(FlatsPerStorey))
dt_6_Story_Areas <- dt_6_Story_Areas %>%
  mutate(CommonAreaPerStorey  = round(CommonAreaPerStorey))

# Select the buildings that have story area between 400-450 m2

lower_limit <- 400
upper_limit <- 450

# Boxplot: CommonAreaPerStorey for TotalAreaPerStorey between
ggplot(dt_6_Story_Areas %>% filter(TotalAreaPerStorey >= lower_limit & TotalAreaPerStorey <= upper_limit),
       aes(x = factor(1), y = CommonAreaPerStorey)) +
  geom_boxplot(fill = "white") +
  labs(title = paste("Core Area Per Storey for the Buildings with Story Area Between 400-450 m2", lower_limit, "and", upper_limit),
       x = "Buildings with the Story Area Between 400-450 m2",
       y = "Core Area Per Storey") +
  theme_gray() +
  geom_text(aes(label = paste(lower_limit, "-", upper_limit)), x = 1, y = upper_limit + 5, size = 5) +
  theme(axis.title.x = element_text(size = 12),  # Change the size of x-axis title
        axis.title.y = element_text(size = 12),  # Change the size of y-axis title
        axis.text.x = element_text(size = 12),  # Change the size of x-axis tick labels
        axis.text.y = element_text(size = 12))  # Change the size of y-axis tick labels

# Boxplot: MetersquarePerFlat for TotalAreaPerStorey between
ggplot(dt_6_Story_Areas %>% filter(TotalAreaPerStorey >= lower_limit & TotalAreaPerStorey <= upper_limit),
       aes(x = factor(1), y = MetersquarePerFlat)) +
  geom_boxplot(fill = "white") +
  labs(title = paste("Area of an Individual Flat for the Buildings with Story Area Between 400-450 m2", lower_limit, "and", upper_limit),
       x = "Buildings with the Story Area Between 400-450 m2",
       y = "Area of an Individual Flat") +
  theme_gray() +
  geom_text(aes(label = paste(lower_limit, "-", upper_limit)), x = 1, y = upper_limit + 5, size = 5) +
  theme(axis.title.x = element_text(size = 12),  # Change the size of x-axis title
        axis.title.y = element_text(size = 12),  # Change the size of y-axis title
        axis.text.x = element_text(size = 12),  # Change the size of x-axis tick labels
        axis.text.y = element_text(size = 12))  # Change the size of y-axis tick labels



