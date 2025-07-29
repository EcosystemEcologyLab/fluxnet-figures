# Load necessary libraries
library(ggplot2)
library(viridis)
library(raster)
library(sf)
library(dplyr)


# Load utility functions
source("R/fcn_utility_FLUXNET.R")

# Load metadata
site_metadata <- load_fluxnet_metadata()

# Load annual FLUXNET data
annual_data <- load_and_clean_annual_data(site_metadata)

# Load AI raster
AI_4km <- raster("~/Documents/RProjects/data/AI_4km.tif")

# Crop to Northern Hemisphere (latitude > 0)
nhem_extent <- extent(xmin(AI_4km), xmax(AI_4km), 30, ymax(AI_4km))
AI_nhem <- crop(AI_4km, nhem_extent)

# Define AI classification
ai_breaks <- c(0,  0.65, 1, 1.75, Inf)


ai_labels <- c(
  "Aridlands - VPD & Stress",
  "Mesic - VPD & Stress",
  "Humid - Phenology & VPD",
  "Energy Limited/ CDH resistent"
)

# Create lookup table
ai_ranges <- data.frame(
  AI_Category = factor(ai_labels, levels = ai_labels),
  AI_Min = head(ai_breaks, -1),
  AI_Max = tail(ai_breaks, -1)
)

# Calculate cell area
AI_area_km2 <- raster::area(AI_nhem)
# Mask area raster to land pixels only (defined by AI raster)
AI_land_area_km2 <- mask(AI_area_km2, AI_nhem)

# Sum only over land
total_land_area_km2_30N <- sum(values(AI_land_area_km2), na.rm = TRUE)


# Summarize by AI category
Area_by_AI <- data.frame(
  AI = values(AI_nhem),
  Area_km2 = values(AI_area_km2)
) %>%
  dplyr::filter(!is.na(AI)) %>%
  dplyr::mutate(
    AI_Category = cut(AI, breaks = ai_breaks, labels = ai_labels, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(AI_Category) %>%
  dplyr::summarise(Total_Area_km2 = sum(Area_km2, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(ai_ranges, by = "AI_Category") %>%
  dplyr::arrange(AI_Min) %>%
  dplyr::mutate(Cumulative_Area_km2 = cumsum(Total_Area_km2)) %>%
  dplyr::mutate(Prop_Area = Cumulative_Area_km2/total_land_area_km2_30N) %>%
  dplyr::select(AI_Category, AI_Min, AI_Max, Total_Area_km2, Cumulative_Area_km2, Prop_Area)

print(Area_by_AI)

# --- Step 5: Plot ---



# Custom color palette (adjust to your liking)

ai_labels <- c(
  "Aridlands - VPD & Stress",
  "Mesic - VPD & Stress",
  "Humid - Phenology & VPD",
  "Energy Limited/ CDH resistent"
)


ai_colors <- c(
  "Aridlands - VPD & Stress" = "#d73027",       # red
  "Mesic - VPD & Stress" = "#fc8d59",           # orange
  "Humid - Phenology & VPD" = "#ffff33",        # light blue
  "Energy Limited/ CDH resistent" = "#66bd63"   # dark blue
)

AI_classified <- raster::cut(AI_nhem, breaks = ai_breaks, include.lowest = TRUE)
AI_classified_downsampled <- aggregate(AI_classified, fact = 4, fun = modal, na.rm = TRUE)


ggplot(AI_df, aes(x = x, y = y, fill = AI_Category)) +
  geom_raster() +
  scale_fill_manual(
    values = ai_colors,
    name = "Aridity Category"
  ) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Aridity Categories (Northern Hemisphere)",
    subtitle = "4 km resolution — Polar Stereographic",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )

polar_equalarea_crs <- CRS("+proj=laea +lat_0=90 +lon_0=0 +datum=WGS84 +units=m +no_defs")
AI_equalarea <- projectRaster(
  AI_classified_downsampled,   # your classified raster, ideally aggregated first
  crs = polar_equalarea_crs,
  method = "ngb"
)


AI_df <- as.data.frame(AI_equalarea, xy = TRUE, na.rm = TRUE)
colnames(AI_df) <- c("x", "y", "class_id")
AI_df$AI_Category <- factor(ai_labels[AI_df$class_id], levels = ai_labels)

ggplot(AI_df, aes(x = x, y = y, fill = AI_Category)) +
  geom_raster() +
  scale_fill_manual(values = ai_colors, name = "Aridity Category") +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "AI Categories in Northern Hemisphere",
    subtitle = "Lambert Azimuthal Equal-Area Projection",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )



# Time varying AI land area
# Lu, J., & Yao, L. (2023). A global Aridity Index raster from 2003 to 2022. Zenodo. https://doi.org/10.5281/zenodo.10074189

# Define time series
years <- 2003:2022
ai_files <- paste0("~/Documents/RProjects/data/Lu_Yao_AITrends/", years, ".tif")

# Define classification scheme
ai_breaks <- c(0, 0.05, 0.2, 0.5, 0.65, 0.8, 1.0, 1.75, Inf)
ai_labels <- c("Hyper-arid", "Arid", "Semi-arid", "Dry sub-humid",
               "Sub-humid", "Humid", "Very humid", "CDH Benefit")

ai_ranges <- data.frame(
  AI_Category = factor(ai_labels, levels = ai_labels),
  AI_Min = head(ai_breaks, -1),
  AI_Max = tail(ai_breaks, -1)
)

area_summary_list <- list()

for (i in seq_along(years)) {
  year <- years[i]
  ai_raster <- raster(ai_files[i])
  ai_nhem <- crop(ai_raster, extent(xmin(ai_raster), xmax(ai_raster), 30, ymax(ai_raster)))
  ai_area_km2 <- raster::area(ai_nhem)
  
  ai_values <- values(ai_nhem)
  area_values <- values(ai_area_km2)
  
  valid <- !is.na(ai_values)
  ai_values <- ai_values[valid]
  area_values <- area_values[valid]
  
  ai_cat <- cut(ai_values, breaks = ai_breaks, labels = ai_labels, include.lowest = TRUE)
  
  df <- data.frame(
    Year = year,
    AI = ai_values,
    Area_km2 = area_values,
    AI_Category = ai_cat
  )
  
  category_summary <- df %>%
    dplyr::group_by(AI_Category) %>%
    dplyr::summarise(Total_Area_km2 = sum(Area_km2), .groups = "drop") %>%
    dplyr::left_join(ai_ranges, by = "AI_Category") %>%
    dplyr::mutate(Year = year)
  
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

# Combine into time series
cdh_df <- do.call(rbind, lapply(area_summary_list, function(x) {
  data.frame(
    Year = x$year,
    CDH_Limited_Fraction = x$cdh_area / x$total_area,
    Below_175_Fraction = x$below_175_area / x$total_area
  )
}))

# Plot CDH Benefit trend
ggplot(cdh_df, aes(x = Year, y = CDH_Limited_Fraction)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(
    title = "Fraction of Northern Hemisphere Land Area in CDH-Benefit Zone",
    y = "Fraction of Total Area",
    x = "Year"
  )

# Plot below 1.75 AI trend
ggplot(cdh_df, aes(x = Year, y = Below_175_Fraction)) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(
    title = "Fraction of Northern Hemisphere Land Area in CDH Impact Zone",
    y = "Fraction of Total Area",
    x = "Year"
  )
