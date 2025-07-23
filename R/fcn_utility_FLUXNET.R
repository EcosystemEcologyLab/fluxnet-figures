# ------------------------
# fcn_utility_FLUXNET.R
# General utility, metadata, and data loading functions for FLUXNET workflow
# ------------------------

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(amerifluxr)
library(ggnewscale)
library(forcats)
library(minpack.lm) # for phenology code
library(patchwork)
library(fs) # for file paths
library(countrycode)

# ------------------------
# Utility Functions
# ------------------------

extract_site <- function(path) {
  str_split(basename(path), "_", simplify = TRUE)[, 2]
}

load_and_cache <- function(patterns, cache_file, extract_site_func) {
  paths <- dir_ls(".", regexp = paste(patterns, collapse = "|"), recurse = TRUE)
  paths <- paths[!grepl("VARINFO", basename(paths))]
  
  if (file.exists(cache_file)) {
    data <- readRDS(cache_file)
    new_paths <- paths[!extract_site_func(paths) %in% unique(data$site)]
    if (length(new_paths) > 0) {
      new_data <- map_df(new_paths, ~ read_csv(.x) %>%
                           mutate(file = basename(.x), .before = 1) %>%
                           mutate(site = extract_site_func(.x)))
      data <- bind_rows(data, new_data)
      saveRDS(data, cache_file)
    }
  } else {
    data <- map_df(paths, ~ read_csv(.x) %>%
                     mutate(file = basename(.x), .before = 1) %>%
                     mutate(site = extract_site_func(.x)))
    saveRDS(data, cache_file)
  }
  return(data)
}

clean_fluxnet_data <- function(data, site_metadata) {
  numeric_cols <- names(select(data, where(is.numeric)))
  data_cleaned <- data %>%
    mutate(across(all_of(numeric_cols), ~ ifelse(. < -9000, NA, .))) %>%
    select(-any_of(names(site_metadata))) %>%
    left_join(site_metadata, by = c("site" = "SITE_ID"))
  return(data_cleaned)
}

add_site_metadata <- function(data, metadata) {
  data %>% select(-any_of(names(metadata))) %>%
    left_join(metadata, by = c("site" = "SITE_ID"))
}

# ------------------------
# Metadata Loading Function
# ------------------------

load_fluxnet_metadata <- function() {
  af_meta <- amf_site_info() %>%
    select(SITE_ID, SITE_NAME, COUNTRY, STATE, IGBP,
           LOCATION_LAT, LOCATION_LONG, LOCATION_ELEV,
           CLIMATE_KOEPPEN, MAT, MAP) %>%
    mutate(DATA_SOURCE = "AmeriFlux")
  
  icos_files <- fs::dir_ls(path = "data", regexp = "ICOSETC_.*_SITEINFO_L2\\.csv$", recurse = TRUE)
  
  icos_meta <- map_dfr(icos_files, function(path) {
    read_csv(path, col_types = cols(SITE_ID = col_character(), GROUP_ID = col_character(),
                                    VARIABLE = col_character(), DATAVALUE = col_character())) %>%
      select(SITE_ID, VARIABLE, DATAVALUE) %>%
      group_by(SITE_ID, VARIABLE) %>%
      summarize(DATAVALUE = first(DATAVALUE), .groups = "drop") %>%
      pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) %>%
      mutate(country_code = str_extract(SITE_ID, "[A-Z]{2}")) |> 
      # Translate 2 letter country codes to english names, falling back on country code if it doesn't work
      mutate(COUNTRY = coalesce(countrycode(country_code, origin = "iso2c", destination = "country.name.en"), country_code)) |> 
      select(-country_code)
  })
  
  icos_meta_clean <- icos_meta %>%
    transmute(
      SITE_ID,
      SITE_NAME,
      # COUNTRY = NA_character_, 
      STATE = NA_character_,
      IGBP,
      LOCATION_LAT = as.numeric(LOCATION_LAT),
      LOCATION_LONG = as.numeric(LOCATION_LONG),
      LOCATION_ELEV = as.numeric(LOCATION_ELEV),
      CLIMATE_KOEPPEN,
      MAT = as.numeric(MAT),
      MAP = as.numeric(MAP),
      DATA_SOURCE = "ICOS"
    )
  
  bind_rows(af_meta, icos_meta_clean) %>%
    distinct(SITE_ID, .keep_all = TRUE)
}

# ------------------------
# Data Loaders
# ------------------------

load_and_clean_daily_data <- function(site_metadata) {
  patterns <- c("FLUXNET_FULLSET_DD.*\\.csv$", "ICOSETC_[^/]+_FLUXNET_DD_L2\\.csv$")
  daily_data <- load_and_cache(patterns, config$daily_cache, extract_site)
  daily_data <- clean_fluxnet_data(daily_data, site_metadata)
  daily_data <- daily_data %>% mutate(date_object = ymd(TIMESTAMP), .before = TIMESTAMP)
  return(daily_data)
}

load_and_clean_annual_data <- function(site_metadata) {
  patterns <- c("FLUXNET_FULLSET_YY.*\\.csv$", "ICOSETC_[^/]+_FLUXNET_YY_L2\\.csv$")
  annual_data <- load_and_cache(patterns, config$annual_cache, extract_site)
  annual_data <- clean_fluxnet_data(annual_data, site_metadata)
  annual_data <- annual_data %>% mutate(year = TIMESTAMP, .before = TIMESTAMP)
  return(annual_data)
}

# ------------------------
# Helper Plot Exporter
# ------------------------

save_plot_list <- function(plot_list, prefix = "plot", out_dir = "saved_plots", width = 8, height = 6) {
  dir.create(out_dir, showWarnings = FALSE)
  for (name in names(plot_list)) {
    file_path <- file.path(out_dir, paste0(prefix, "_", name, ".png"))
    ggsave(file_path, plot_list[[name]], width = width, height = height, dpi = 300)
  }
}
