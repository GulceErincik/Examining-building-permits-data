##
# Author: Gulce Erincik
# Created: 05.24.2024
# 
# Description: Conventional Wall Materials in Istanbul - examination using the Building Permits Data from TUIK. 
##

install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

library("readxl")
library("ggplot2")
library("dplyr")

# ================================================
# Read xlsx files (yapi kullanma izin belgelerine gore veriler)
# ================================================

all_data <- read_excel("TuikConstructionMaterials.xlsx")
glimpse(all_data)

# ================================================
# Prepare data
# ================================================

# Select columns and specific values for new data set
selected_data <- all_data %>%
  select(...2, ...5, ...7, ...8, ...9, ...10, ...11, ...12, ...13, ...14, ...15, ...16) %>% 
  filter(...5 %in% c("Maltepe") & ...7 %in% c("İki ve daha fazla daireli binalar")) %>% 
  rename("Year" = ...2, "Municipality" = ...5, "PurposeOfUse" = ...7, "NumberOfStoreys" = ...8, "StructuralSystem" = ...9, "WallMaterial" = ...10, "NumberOfBuildings" = ...11, "NumberOfFlats" = ...12, "TotalArea" = ...13, "TotalFlatArea" = ...14, "TotalOtherArea" = ...15, "TotalCoreArea" = ...16)

# Omit NAs
selected_data_clean <- na.omit(selected_data)

# Check for NAs in NumberOfStoreys before omitting NAs
print(any(is.na(selected_data_clean)))

# Adjust classes of variables
class(selected_data_clean$NumberOfBuildings)
selected_data_clean$NumberOfBuildings <- as.numeric(selected_data_clean$NumberOfBuildings)

# ================================================
# Total Number of buildings per wall materials
# ================================================

# Group data by WallMaterials and count the total number of buildings
total_buildings_by_wall_material <- selected_data_clean %>%
  group_by(WallMaterial) %>%
  summarize(TotalBuildings = sum(NumberOfBuildings, na.rm = TRUE))

# Print the result
print(total_buildings_by_wall_material)

# ================================================
# Visualize Distribution of Wall Materials
# ================================================

# Calculate percentages for each wall material
total_buildings_by_wall_material <- total_buildings_by_wall_material %>%
  mutate(Percentage = TotalBuildings / sum(TotalBuildings) * 100) %>%
  mutate(WallMaterial = factor(WallMaterial, levels = c("Beton blok", "Briket", "Gazbeton", "Tuğla")))

# Create a bar plot for Maltepe-Materials with percentage labels on x-axis
ggplot(total_buildings_by_wall_material, aes(x = WallMaterial, y = TotalBuildings)) +
  geom_bar(stat = "identity", fill = "#F5C464") +
  geom_text(aes(label = paste0(sprintf("%.2f", Percentage), "%")), vjust = -0.5, size = 3.5, fontface = "bold") +  # Add percentage labels
  labs(title = "Total Number of Buildings per Wall Material for Multifamily Apartments",
       x = "Wall Material",
       y = "Total Number of Buildings") +
  scale_x_discrete(labels = c("Beton blok" = "Concrete Block",
                              "Briket" = "Block Bims",
                              "Gazbeton" = "Aerated Concrete",
                              "Tuğla" = "Brick (Hollow Clay Tiles)")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black"),  # Keep the original format and order
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_text(hjust = 0, vjust = 0),  # Adjust the position of x-axis title
        axis.title.y = element_text(hjust = 0, vjust = 2),  # Adjust the position of y-axis title
        plot.title = element_text(size = 10, face = "bold"))

