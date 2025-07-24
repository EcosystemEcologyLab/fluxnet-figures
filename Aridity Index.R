# Load necessary libraries
library(ggplot2)
library(viridis)
library(raster)
library(sf)
library(dplyr)
# main_analysis.R

# Load the utility functions
source("R/fcn_utility_FLUXNET.R")

# Load metadata first
site_metadata <- load_fluxnet_metadata()

# Then load data using the metadata
#daily_data <- load_and_clean_daily_data(site_metadata)
annual_data <- load_and_clean_annual_data(site_metadata)

# Load AI raster
AI_4km <- raster("~/Documents/RProjects/data/AI_4km.tif")

# Crop to Northern Hemisphere (latitude > 0)
nhem_extent <- extent(xmin(AI_4km), xmax(AI_4km), 0, ymax(AI_4km))
AI_nhem <- crop(AI_4km, nhem_extent)

# Define AI bands and labels
ai_breaks <- c(0, 0.05, 0.2, 0.5, 0.65, 0.8, 1.75, 2.5, Inf)
ai_labels <- c("Hyper-arid", "Arid", "Semi-arid", "Dry sub-humid", 
               "Sub-humid", "Humid", "Very humid", "CDH limited")

# Create lookup table with AI range information
ai_ranges <- data.frame(
  AI_Category = factor(ai_labels, levels = ai_labels),
  AI_Min = head(ai_breaks, -1),
  AI_Max = tail(ai_breaks, -1)
)

# Calculate cell area (in km²) for each cell
AI_area_km2 <- area(AI_nhem)

# Build a data frame of AI values and corresponding cell areas
Area_by_AI <- data.frame(
  AI = values(AI_nhem),
  Area_km2 = values(AI_area_km2)
) %>%
  filter(!is.na(AI)) %>%
  mutate(AI_Category = cut(AI, breaks = ai_breaks, labels = ai_labels, include.lowest = TRUE)) %>%
  group_by(AI_Category) %>%
  summarise(Total_Area_km2 = sum(Area_km2, na.rm = TRUE), .groups = "drop") %>%
  left_join(ai_ranges, by = "AI_Category") %>%
  select(AI_Category, AI_Min, AI_Max, Total_Area_km2)

# View the result
print(Area_by_AI)


# Time varying AI land area
# Lu, J., & Yao, L. (2023). A global Aridity Index raster from 2003 to 2022. Zenodo. https://doi.org/10.5281/zenodo.10074189


# Define the years you want to analyze
years <- 2003:2022

# Define file paths
ai_files <- paste0("~/Documents/RProjects/data/Lu_Yao_AITrends/", years, ".tif")

# Define AI classification breaks and labels
ai_breaks <- c(0, 0.05, 0.2, 0.5, 0.65, 0.8, 1.0, 1.75, Inf)
ai_labels <- c("Hyper-arid", "Arid", "Semi-arid", "Dry sub-humid",
               "Sub-humid", "Humid", "Very humid", "CDH Benefit")

# Create lookup table
ai_ranges <- data.frame(
  AI_Category = factor(ai_labels, levels = ai_labels),
  AI_Min = head(ai_breaks, -1),
  AI_Max = tail(ai_breaks, -1)
)

# Initialize list to store results
area_summary_list <- list()

# Loop over files
for (i in seq_along(years)) {
  year <- years[i]
  ai_raster <- raster(ai_files[i])
  
  # Crop to Northern Hemisphere
  nhem_extent <- extent(xmin(ai_raster), xmax(ai_raster), 0, ymax(ai_raster))
  ai_nhem <- crop(ai_raster, nhem_extent)
  
  # Calculate area (km²)
  ai_area_km2 <- area(ai_nhem)
  
  # Get values
  ai_values <- values(ai_nhem)
  area_values <- values(ai_area_km2)
  
  # Filter valid cells
  valid <- !is.na(ai_values)
  ai_values <- ai_values[valid]
  area_values <- area_values[valid]
  
  # Categorize
  ai_cat <- cut(ai_values, breaks = ai_breaks, labels = ai_labels, include.lowest = TRUE)
  
  # Create data frame
  df <- data.frame(
    Year = year,
    AI = ai_values,
    Area_km2 = area_values,
    AI_Category = ai_cat
  )
  
  # Summarize by category
  category_summary <- df %>%
    group_by(AI_Category) %>%
    summarise(Total_Area_km2 = sum(Area_km2), .groups = "drop") %>%
    left_join(ai_ranges, by = "AI_Category") %>%
    mutate(Year = year)
  
  # Also calculate CDH-limited and <1.75
  total_area <- sum(df$Area_km2, na.rm = TRUE)
  cdh_area <- sum(df$Area_km2[df$AI > 1.75], na.rm = TRUE)
  below_175_area <- sum(df$Area_km2[df$AI <= 1.75], na.rm = TRUE)
  
  area_summary_list[[i]] <- list(
    category = category_summary,
    total_area = total_area,
    cdh_area = cdh_area,
    below_175_area = below_175_area,
    year = year
  )
}

# Combine time series data
cdh_df <- do.call(rbind, lapply(area_summary_list, function(x) {
  data.frame(
    Year = x$year,
    CDH_Limited_Fraction = x$cdh_area / x$total_area,
    Below_175_Fraction = x$below_175_area / x$total_area
  )
}))

# Plot CDH limited over time
ggplot(cdh_df, aes(x = Year, y = CDH_Limited_Fraction)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Fraction of Northern Hemisphere Land Area in CDH-Benefit Zone",
    y = "Fraction of Total Area",
    x = "Year"
  )

# Plot All AI <= 1.75 over time
ggplot(cdh_df, aes(x = Year, y = Below_175_Fraction)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(
    title = "Fraction of Northern Hemisphere Land Area in CDH Impact Zone",
    y = "Fraction of Total Area",
    x = "Year"
  )
