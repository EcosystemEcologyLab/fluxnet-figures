# ------------------------
# fcn_utility_FLUXNET.R
# General utility, metadata, and data loading functions for FLUXNET workflow
# ------------------------

# Load required libraries
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
library(dplyr)

# ------------------------
# Configuration (Global)
# ------------------------

config <- list(
  daily_cache = "data/multiple_sites_daily.rds",
  annual_cache = "data/multiple_sites_annual.rds"
)

# ------------------------
# Utility Functions
# ------------------------

library(fs)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)

#########
######### discover_AMF_files
######### to find out how many AMF fluxnet files are available 
# Example usage:
# amf_manifest <- discover_AMF_files("data")
discover_AMF_files <- function(data_dir = "data") {
  library(fs)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  
  # 1. List all CSVs
  all_csvs <- fs::dir_ls(path = data_dir, recurse = TRUE, regexp = "\\.csv$")
  
  # 2. Filter to AMF FULLSET FLUXNET
  amf_files <- dir_ls(data_dir, recurse = TRUE, regexp = "\\.csv$") %>%
    # only those starting with AMF_ and containing the full pattern
    keep(~ str_detect(basename(.x),
                      "^AMF_[^_]+_FLUXNET_FULLSET_(?:YY|DD|HH|WW)_[0-9]{4}-[0-9]{4}_.*\\.csv$")
    )

  
  

  
  
  # 3. Build the manifest
  manifest <- tibble(path = amf_files) %>%
    mutate(filename = basename(path)) %>%
    tidyr::extract(
      col     = "filename",
      into    = c("data_center", "site", "data_product", "dataset",
                  "time_integral", "start_year", "end_year"),
      regex   = "^(AMF)_([^_]+)_([^_]+)_([^_]+)_(YY|DD|HH|WW)_([0-9]{4})-([0-9]{4})_.*\\.csv$",
      remove  = FALSE,
      convert = TRUE
    )
  
  # 4. Print summary per integral
  manifest %>%
    group_by(time_integral) %>%
    summarise(
      unique_sites = n_distinct(site),
      total_years  = sum(end_year - start_year + 1),
      n_files      = n(),
      .groups      = "drop"
    ) %>%
    arrange(time_integral) %>%
    rowwise() %>%
    do({
      cat(
        sprintf(
          "• %s files → %d unique sites, %d total site-years across %d files\n",
          .$time_integral, .$unique_sites, .$total_years, .$n_files
        )
      )
      tibble() 
    })
  
  # 5. Return the manifest data frame
  manifest
}


#######
#######
#######


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
  # 1) Identify numeric columns for sentinel cleaning
  numeric_cols <- names(dplyr::select(data, where(is.numeric)))
  
  # 2) Drop metadata columns *other* than SITE_ID, and never drop 'site'
  cols_to_drop <- setdiff(
    names(site_metadata),          # all metadata names
    c("SITE_ID", "site")           # keep these two
  )
  
  data %>%
    # (1) sentinel conversion
    mutate(across(all_of(numeric_cols), ~ ifelse(. < -9000, NA, .))) %>%
    # (2) drop unwanted metadata cols but keep data$site
    select(-any_of(cols_to_drop)) %>%
    # (3) join
    left_join(site_metadata, by = c("site" = "SITE_ID")) %>%
    # (4) drop the replicated metadata's "site" column
    select(-site.y)
}


add_site_metadata <- function(data, metadata) {
  data %>% dplyr::select(-any_of(names(metadata))) %>%
    left_join(metadata, by = c("site" = "SITE_ID"))
}

# ------------------------
# Metadata Loading Function
# ------------------------

load_fluxnet_metadata <- function() {
  af_meta <- amf_site_info() %>%
    dplyr::select(SITE_ID, SITE_NAME, COUNTRY, STATE, IGBP,
                  LOCATION_LAT, LOCATION_LONG, LOCATION_ELEV,
                  CLIMATE_KOEPPEN, MAT, MAP) %>%
    mutate(DATA_SOURCE = "AmeriFlux")
  
  icos_files <- fs::dir_ls(path = "data", regexp = "ICOSETC_.*_SITEINFO_L2\\.csv$", recurse = TRUE)
  
  icos_meta <- map_dfr(icos_files, function(path) {
    read_csv(path, col_types = cols(SITE_ID = col_character(), GROUP_ID = col_character(),
                                    VARIABLE = col_character(), DATAVALUE = col_character())) %>%
      dplyr::select(SITE_ID, VARIABLE, DATAVALUE) %>%
      group_by(SITE_ID, VARIABLE) %>%
      summarize(DATAVALUE = first(DATAVALUE), .groups = "drop") %>%
      pivot_wider(names_from = VARIABLE, values_from = DATAVALUE) %>%
      mutate(country_code = str_extract(SITE_ID, "[A-Z]{2}")) %>%
      # Translate 2-letter country codes to English names, fallback to code
      mutate(COUNTRY = coalesce(countrycode(country_code, origin = "iso2c", destination = "country.name.en"), country_code)) %>%
      dplyr::select(-country_code)
  })
  
  icos_meta_clean <- icos_meta %>%
    transmute(
      SITE_ID,
      SITE_NAME,
      STATE = NA_character_,
      COUNTRY,
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
    distinct(SITE_ID, .keep_all = TRUE) %>%
    mutate(site = SITE_ID) %>%
    mutate(SITEID = SITE_ID)
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
