

# ------------------------
# Load Libraries
# ------------------------
library(dplyr)
library(ggplot2)
library(zyp)     # Mann-Kendall trend test
library(purrr)
library(trend)
library(countrycode)
library(tidyr)
library(forcats)


# Source utility and plotting scripts
source("R/fcn_utility_FLUXNET.R")
source("R/fcn_plot_FLUXNET.R")
source("R/fcn_IPCC_FLUXNET.R")
source("R/fcn_IPCC_plots_FLUXNET.R")

# Load metadata
site_metadata <- load_fluxnet_metadata()
# Load and clean data
annual_data <- annual

# annual_data <- annual_data%>%
#   mutate(
#     Alpha2 = substr(site, 1, 2),
#     Continent = countrycode(Alpha2, origin = 'iso2c', destination = 'continent')
#   )
#daily_data  <- load_and_clean_daily_data(site_metadata)

# Prepare data
#shapefile_path <- "data/gez2010/gez_2010_wgs84.shp"

ipcc_site_metadata <- prepare_fluxnet_for_ipcc(
  site_metadata,
  gez_shapefile_path = "data/gez2010/gez_2010_wgs84.shp",
  ipcc_csv_path = "data/IPCCEFDB/IPCC_Regions_Countries.csv",
  iso_csv_path = "data/ISOCountryCodes.csv"
)


expected_vars <- c("Alpha2_country", "Continent", "Country_name", "ClassID_SitePI",
                   "gez_name", "standardized_gez_name", "IPCC_Region")

missing_vars <- setdiff(expected_vars, names(fluxnet_for_ipcc))
if (length(missing_vars) > 0) {
  warning("The following expected variables are missing from fluxnet_for_ipcc: ", paste(missing_vars, collapse = ", "))
} else {
  message("All expected variables successfully added.")
}

#### some diagnostic checks
# Ensure NEE_transformed exists
if (!"NEE_transformed" %in% colnames(fluxnet_for_ipcc)) {
  fluxnet_for_ipcc <- fluxnet_for_ipcc %>%
    mutate(NEE_transformed = -NEE_VUT_REF * 0.01)
}

# Plot NEE_transformed by Continent with proper y-axis label
plot_diagnostic_boxplot(
  data = fluxnet_for_ipcc,
  group_var = "Country",
  y_var = "NEE_transformed",
  y_label = expression(NEP~(MgC~ha^{-1}~yr^{-1}))
)


# List of expected variables to group by
group_vars <- c(
  "Continent",
  "IPCC_Region",
  "Country_name",
  "ClassID_SitePI",
  "gez_name",
  "standardized_gez_name",
  "Alpha2_country"
)
# Loop through each variable and generate the plot
for (var in group_vars) {
  message("Plotting NEE_transformed by ", var, "...")
  
  p <- plot_diagnostic_boxplot(
    data = fluxnet_for_ipcc,
    group_var = var,
    y_var = "NEE_transformed",
    y_label = expression(NEP~(MgC~ha^{-1}~yr^{-1}))
  )
  print(p)  # Explicitly print the ggplot object
}

load(file = "data/IPCCEFDB/IPCC_EF_MgC_ha_y_jul2024.Rdata")

 IPCC_EF = IPCC_EF_MgC_ha_y_jul2024
  
 
 load("data/IPCCEFDB/IPCC_EF_MgC_ha_y_jul2024.Rdata")
 IPCC_EF <- IPCC_EF_MgC_ha_y_jul2024 %>%
   rename(standardized_gez_name = ecological_zone)
 
 # Clean and standardize 'standardized_gez_name' in IPCC_EF
 IPCC_EF <- IPCC_EF %>%
   mutate(standardized_gez_name = str_to_title(str_replace_all(standardized_gez_name, "_", " ")))
 
 # Ensure 'standardized_gez_name' in fluxnet_data matches IPCC_EF
 fluxnet_for_ipcc <- fluxnet_for_ipcc %>%
   mutate(standardized_gez_name = str_to_title(str_replace_all(standardized_gez_name, "_", " ")))
 
 # Perform analysis by Continent
 continent_results <- perform_analysis(fluxnet_for_ipcc, IPCC_EF, "Continent")
 
 perform_analysis (fluxnet_for_ipcc, IPCC_EF, "IGBP")
 
# IPCC_EF <- IPCC_EF_MgC_ha_y_jul2024 %>%
#   rename(gez_name = ecological_zone) %>%
#   mutate(Continent = case_when(
#     continent == "north_and_south_america" ~ "Americas",
#     continent == "europe" ~ "Europe",
#     continent %in% c("asia", "asia_continental", "asia_insular") ~ "Asia",
#     continent %in% c("africa", "") ~ "Africa",
#     continent == "new_zealand" ~ "Oceania",
#     continent == "asia_europe_northamerica" ~ "Asia_Europe_NorthAmerica",
#     TRUE ~ NA_character_
#   ))


 # Perform analysis by Continent
 continent_results <- perform_analysis(fluxnet_data, IPCC_EF, "Continent")
 
 # Generate and save plots for Continents
 for (continent in names(continent_results)) {
   comparison_table <- continent_results[[continent]]
   if (nrow(comparison_table) == 0) next  # Skip if no data
   
   p <- plot_comparison(comparison_table, "Continent", continent)
   ggsave(
     filename = paste0("IPCC/plots/Difference_IPCC_FLUXNET_", continent, ".png"),
     plot = p,
     width = 10,
     height = 7
   )
 }
 
 # Perform analysis by IPCC Region
 ipcc_region_results <- perform_analysis(fluxnet_data, IPCC_EF, "IPCC_Region")
 
 # Generate and save plots for IPCC Regions
 for (region in names(ipcc_region_results)) {
   comparison_table <- ipcc_region_results[[region]]
   if (nrow(comparison_table) == 0) next  # Skip if no data
   
   p <- plot_comparison(comparison_table, "IPCC Region", region)
   ggsave(
     filename = paste0("IPCC/plots/Difference_IPCC_FLUXNET_", region, ".png"),
     plot = p,
     width = 10,
     height = 7
   )
 }
 
 # Perform original analysis
 analysis_results <- perform_original_analysis(fluxnet_for_ipcc, IPCC_EF)
 
 # Generate plots
 p1_boxplot_only <- plot_emission_factor_boxplot(analysis_results$fluxnet_data, IPCC_EF)
 ggsave(filename = "IPCC/plots/Emission_Factor_Boxplot.png", plot = p1_boxplot_only, width = 10, height = 8, dpi = 300)
 
 p_difference <- plot_difference(analysis_results$merged_data)
 ggsave(filename = "IPCC/plots/Difference_Plot.png", plot = p_difference, width = 10, height = 8, dpi = 300)
 
 p_EF_Bias_values <- plot_EF_Bias_values(analysis_results$merged_data)
 ggsave(filename = "IPCC/plots/EF_Bias_Values.png", plot = p_EF_Bias_values, width = 10, height = 8, dpi = 300)

 p1_boxplot_allvegV <- ggplot(analysis_results$fluxnet_data, aes(x = standardized_gez_name, y = NEE_transformed)) +
   geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "lightgrey", colour = "black") +
   geom_point(data = IPCC_EF, aes(x = standardized_gez_name, y = IPCC_MgC_ha_y), color = "cyan", size = 3) +
   labs(
     title = "Emission Factor Distribution by FAO Global Ecological Zones",
     y = expression("NEP (Mg C ha"^{-1}~"yr"^{-1}*")")
   ) +
   consistent_theme_vertical()
 
 
 
 
 IPCC_EF <- IPCC_EF %>%
   rename(Continent = continent)
 
 IPCC_EF <- IPCC_EF %>%
   mutate(
     Continent = case_when(
       Continent %in% c("north_and_south_america", "asia_europe_northamerica") ~ "Americas",
       Continent %in% c("asia", "asia_continental", "asia_insular") ~ "Asia",
       Continent == "africa" ~ "Africa",
       Continent == "europe" ~ "Europe",
       Continent == "new_zealand" ~ "Oceania",
       TRUE ~ NA_character_
     ),
     standardized_gez_name = str_to_title(str_replace_all(gez_name, "_", " "))
   )
 
 
 # Ensure transformed NEE exists
 fluxnet_for_ipcc <- fluxnet_for_ipcc %>%
   mutate(NEE_transformed = -NEE_VUT_REF * 0.01)
 
 # Forest class codes
 forest_classes <- c("ENF", "EBF", "DNF", "DBF", "MF")
 
 # Loop through each continent
 for (cont in unique(na.omit(fluxnet_for_ipcc$Continent))) {
   
   # Filter FLUXNET and IPCC EF data for this continent
   FLUXNETFAOgez_cont <- fluxnet_for_ipcc %>%
     filter(Continent == cont)
   
   IPCC_EF_cont <- IPCC_EF %>%
     filter(Continent == cont)
   
   # File prefix for saving plots
   file_prefix <- paste0("plots/", gsub(" ", "_", cont), "_")
   
   # --- 1. All Vegetation: Vertical Boxplot ---
   p_vertical_boxplot <- ggplot(FLUXNETFAOgez_cont, aes(x = standardized_gez_name, y = NEE_transformed)) +
     geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "lightgrey", colour = "black") +
     geom_point(data = IPCC_EF_cont, aes(x = standardized_gez_name, y = IPCC_MgC_ha_y), color = "cyan", size = 4) +
     labs(title = paste("Emission Factor Distribution -", cont), y = "NEP (MgC ha⁻¹ yr⁻¹)") +
     consistent_theme_vertical()
   print(p_vertical_boxplot)
   ggsave(filename = paste0(file_prefix, "vertical_boxplot.png"), plot = p_vertical_boxplot, width = 8, height = 6)
   
   # --- 2. All Vegetation: Horizontal Boxplot ---
   p_horizontal_boxplot <- ggplot(FLUXNETFAOgez_cont, aes(y = standardized_gez_name, x = NEE_transformed)) +
     geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "lightgrey", colour = "black") +
     geom_point(data = IPCC_EF_cont, aes(y = standardized_gez_name, x = IPCC_MgC_ha_y), color = "cyan", size = 4) +
     labs(title = paste("Emission Factor Distribution -", cont), x = "NEP (MgC ha⁻¹ yr⁻¹)") +
     consistent_theme_horizontal()
   print(p_horizontal_boxplot)
   ggsave(filename = paste0(file_prefix, "horizontal_boxplot.png"), plot = p_horizontal_boxplot, width = 8, height = 6)
   
   # --- 3. Forest-Only: Vertical Boxplot ---
   forest_data_cont <- FLUXNETFAOgez_cont %>% filter(IGBP %in% forest_classes)
   if (nrow(forest_data_cont) > 0) {
     p_vertical_forest_boxplot <- ggplot(forest_data_cont, aes(x = standardized_gez_name, y = NEE_transformed)) +
       geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "white", colour = "black") +
       geom_point(data = IPCC_EF_cont, aes(x = standardized_gez_name, y = IPCC_MgC_ha_y), color = "cyan", size = 4) +
       labs(title = paste("Forest-Only Emission Factor Distribution -", cont), y = "NEP (MgC ha⁻¹ yr⁻¹)") +
       consistent_theme_vertical()
     print(p_vertical_forest_boxplot)
     ggsave(filename = paste0(file_prefix, "vertical_forest_boxplot.png"), plot = p_vertical_forest_boxplot, width = 8, height = 6)
     
     # --- 4. Forest-Only: Horizontal Boxplot ---
     p_horizontal_forest_boxplot <- ggplot(forest_data_cont, aes(y = standardized_gez_name, x = NEE_transformed)) +
       geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "white", colour = "black") +
       geom_point(data = IPCC_EF_cont, aes(y = standardized_gez_name, x = IPCC_MgC_ha_y), color = "cyan", size = 4) +
       labs(title = paste("Forest-Only Emission Factor Distribution -", cont), x = "NEP (MgC ha⁻¹ yr⁻¹)") +
       consistent_theme_horizontal()
     print(p_horizontal_forest_boxplot)
     ggsave(filename = paste0(file_prefix, "horizontal_forest_boxplot.png"), plot = p_horizontal_forest_boxplot, width = 8, height = 6)
   }
 }
 
 
 
 
 
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Load GEZ shapefile
gez <- st_read(shapefile_path, quiet = TRUE) %>%
  st_transform(4326)

# Load world country borders for context
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for fluxnet sites that successfully joined (i.e., have non-NA GEZ)
fluxnet_joined <- fluxnet_for_ipcc %>%
  filter(!is.na(gez_name)) %>%
  distinct(site, Longitude = LOCATION_LONG, Latitude = LOCATION_LAT, gez_name)

# Convert fluxnet site coordinates to sf
fluxnet_sf <- st_as_sf(fluxnet_joined, coords = c("Longitude", "Latitude"), crs = 4326)

# Create the map
ggplot() +
  geom_sf(data = gez, aes(fill = gez_name), color = NA) +
  geom_sf(data = world, fill = NA, color = "black", size = 0.2) +
  geom_sf(data = fluxnet_sf, shape = 21, color = "black", fill = "white", size = 2, stroke = 0.5) +
  coord_sf(expand = FALSE) +
  labs(title = "FAO Global Ecological Zones with FLUXNET Site Overlays",
       subtitle = "Only sites successfully joined to a GEZ region are shown - fuzzies") +
  theme_minimal() +
  theme(legend.position = "none")
