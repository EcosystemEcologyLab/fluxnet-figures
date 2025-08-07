
# R/fcn_IPCC_FLUXNET.R
#Functions to carry out the IPCC analysis
# ------------------------
# Prepare FLUXNET for IPCC
# ------------------------


prepare_fluxnet_for_ipcc <- function(annual_data, gez_shapefile_path, ipcc_csv_path, iso_csv_path) {
  library(dplyr)
  library(sf)
  library(countrycode)
  library(stringdist)
  library(stringr)
  library(tibble)
  
  # ---- Helper: Clean country names ----
  clean_country_name <- function(name) {
    name %>%
      trimws() %>%
      tolower() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      sub("^the\\s+", "", .)
  }
  
  # ---- Helper: Fuzzy match IPCC country names to ISO ----
  find_closest_match <- function(country, reference_list, max_distance = 0.15) {
    if (is.na(country) || length(reference_list) == 0) return(NA)
    distances <- stringdist(country, reference_list, method = "jw")
    if (all(is.na(distances))) return(NA)
    min_distance <- min(distances, na.rm = TRUE)
    closest_match <- reference_list[which.min(distances)]
    if (min_distance <= max_distance) return(closest_match) else return(NA)
  }
  
  # ---- Step 1: Assign Alpha2 codes and Continent ----
  annual_data <- annual_data %>%
    mutate(
      Alpha2_country = substr(site, 1, 2),
      Alpha2_country = recode(Alpha2_country, "UK" = "GB", "SP" = "ES"),
      Continent = countrycode(Alpha2_country, origin = "iso2c", destination = "continent"),
      Country_name = COUNTRY,
      ClassID_SitePI = IGBP
    )
  
  # ---- Step 2: GEZ spatial join ----
  original_s2_setting <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  
  site_coords <- annual_data %>%
    distinct(site, Latitude = LOCATION_LAT, Longitude = LOCATION_LONG) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  FAOgez_data <- st_read(gez_shapefile_path, quiet = TRUE) %>%
    st_transform(4326) %>%
    st_make_valid()
  
  site_with_gez <- st_join(site_coords, FAOgez_data["gez_name"], left = TRUE)
  sf::sf_use_s2(original_s2_setting)
  
  annual_data <- left_join(annual_data, st_drop_geometry(site_with_gez), by = "site") %>%
    mutate(standardized_gez_name = str_to_title(str_replace_all(gez_name, "_", " ")))
  
  # ---- Step 3: IPCC region mapping ----
  
  # Load and clean reference data
  IPCC_Regions_Countries <- read.csv(ipcc_csv_path, stringsAsFactors = FALSE) %>%
    rename(Country_name = Country) %>%
    mutate(Country_name = clean_country_name(Country_name))
  
  iso_country_codes <- read.csv(iso_csv_path, stringsAsFactors = FALSE) %>%
    mutate(Country_name = clean_country_name(Country_name))
  
  # Fuzzy match IPCC to ISO
  IPCC_Regions_Countries <- IPCC_Regions_Countries %>%
    rowwise() %>%
    mutate(Matched_Country_name = find_closest_match(Country_name, iso_country_codes$Country_name)) %>%
    ungroup()
  
  # Merge with ISO codes
  ipcc_lookup <- left_join(IPCC_Regions_Countries, iso_country_codes, by = c("Matched_Country_name" = "Country_name")) %>%
    select(Alpha2_country, IPCC_Region)
  
  # Manual overrides
  manual_mappings <- tibble(
    Alpha2_country = c("GF", "GL", "SJ", "NL"),
    Matched_Country_name = c("guyana", "greenland", "svalbard and jan mayen", "netherlands"),
    IPCC_Region = c("Latin America and Caribbean", "Europe", "Europe", "Europe")
  )
  
  ipcc_lookup <- bind_rows(ipcc_lookup, manual_mappings) %>%
    distinct(Alpha2_country, .keep_all = TRUE)
  
  # Merge IPCC region into main dataset
  annual_data <- left_join(annual_data, ipcc_lookup, by = "Alpha2_country")
  
  return(annual_data)
}



# Load necessary libraries
library(dplyr)
library(stringr)

# Function to perform analysis for a given grouping variable
perform_analysis <- function(fluxnet_data, IPCC_EF, group_var) {
  results_list <- list()
  groups <- unique(fluxnet_data[[group_var]])
  groups <- groups[!is.na(groups)]  # Remove NA values
  
  for (group in groups) {
    data_subset <- fluxnet_data %>% filter((!!sym(group_var)) == group)
    if (nrow(data_subset) == 0) next  # Skip if no data
    
    # Calculate means and number of years per site
    fluxnet_means <- data_subset %>%
      group_by(site) %>%
      summarise(
        Mean_NEE_VUT_REF = mean(NEE_VUT_REF, na.rm = TRUE),
        NumYrs = n_distinct(TIMESTAMP[!is.na(NEE_VUT_REF)]),
        standardized_gez_name = first(standardized_gez_name),
        .groups = 'drop'
      )
    
    # Transform NEE to match IPCC units
    fluxnet_means <- fluxnet_means %>%
      mutate(NEE_transformed = -Mean_NEE_VUT_REF * 0.01)
    
    # Aggregate by GEZ
    summary_stats <- fluxnet_means %>%
      group_by(standardized_gez_name) %>%
      summarise(
        mean_NEE_transformed = mean(NEE_transformed, na.rm = TRUE),
        sd_NEE_transformed = sd(NEE_transformed, na.rm = TRUE),
        n_NEE = n(),
        .groups = 'drop'
      )
    
    # Merge with IPCC EF
    comparison_table <- summary_stats %>%
      left_join(IPCC_EF, by = "standardized_gez_name") %>%
      mutate(Difference = IPCC_MgC_ha_y - mean_NEE_transformed)
    
    # Store results
    results_list[[group]] <- comparison_table
  }
  
  return(results_list)
}


# Function to perform the original analysis
perform_original_analysis <- function(fluxnet_data, IPCC_EF) {
  # Ensure necessary columns are present
  if (!'NEE_VUT_REF' %in% names(fluxnet_data)) {
    stop("Error: 'NEE_VUT_REF' not found in fluxnet_data.")
  }
  
  # Calculate means and number of years per site
  fluxnet_means <- fluxnet_data %>%
    group_by(site) %>%
    summarise(
      Mean_NEE_VUT_REF = mean(NEE_VUT_REF, na.rm = TRUE),
      NumYrs = n_distinct(TIMESTAMP[!is.na(NEE_VUT_REF)]),
      standardized_gez_name = first(standardized_gez_name),
      .groups = 'drop'
    )
  
  # Transform NEE to match IPCC units
  fluxnet_means <- fluxnet_means %>%
    mutate(NEE_transformed = -Mean_NEE_VUT_REF * 0.01)
  
  # Aggregate the mean values of NEE_transformed by standardized_gez_name
  mean_NEE <- fluxnet_means %>%
    group_by(standardized_gez_name) %>%
    summarise(
      mean_NEE_transformed = mean(NEE_transformed, na.rm = TRUE),
      sd_NEE_transformed = sd(NEE_transformed, na.rm = TRUE),
      n_NEE = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      se_NEE = sd_NEE_transformed / sqrt(n_NEE),
      lower_CI_NEE = mean_NEE_transformed - 1.96 * se_NEE,
      upper_CI_NEE = mean_NEE_transformed + 1.96 * se_NEE
    )
  
  # Prepare IPCC data
  mean_IPCC <- IPCC_EF %>%
    group_by(standardized_gez_name) %>%
    summarise(
      mean_IPCC_MgC_ha_y = mean(IPCC_MgC_ha_y, na.rm = TRUE),
      sd_IPCC_MgC_ha_y = sd(IPCC_MgC_ha_y, na.rm = TRUE),
      n_IPCC = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      se_IPCC = sd_IPCC_MgC_ha_y / sqrt(n_IPCC),
      lower_CI_IPCC = mean_IPCC_MgC_ha_y - 1.96 * se_IPCC,
      upper_CI_IPCC = mean_IPCC_MgC_ha_y + 1.96 * se_IPCC
    )
  
  # Merge data
  merged_data <- mean_NEE %>%
    left_join(mean_IPCC, by = "standardized_gez_name") %>%
    mutate(Difference = mean_IPCC_MgC_ha_y - mean_NEE_transformed)
  
  return(list(fluxnet_data = fluxnet_means, merged_data = merged_data))
}