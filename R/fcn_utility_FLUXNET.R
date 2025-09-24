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

### Unzip
  # out <- unzip_fluxnet_zips("data/FLUXNET/AMF")
  # # all the CSVs:
  # head(out$successes)
  # # what zipped files didn’t unzip (and why):
  # out$failures  
unzip_fluxnet_zips <- function(location,
                               exdir     = file.path(location, "unzipped"),
                               overwrite = TRUE) {
  library(fs); library(stringr); library(dplyr); library(purrr); library(tibble)
  
  zip_paths <- fs::dir_ls(location, recurse = TRUE, regexp = "\\.zip$")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  
  # prepare empty failures list
  failures <- tibble(
    zip   = character(),
    stage = character(),
    error = character()
  )
  
  # process each ZIP
  successes <- map_dfr(zip_paths, function(zf) {
    fname <- basename(zf)
    parts <- strsplit(tools::file_path_sans_ext(fname), "_")[[1]]
    if (length(parts) < 5) {
      failures <<- add_row(failures,
                           zip   = fname,
                           stage = "name-parse",
                           error = "unexpected filename structure")
      return(NULL)
    }
    
    data_center  <- parts[1]
    site         <- parts[2]
    data_product <- parts[3]
    dataset      <- parts[4]
    yrs          <- tryCatch(strsplit(parts[5], "-")[[1]],
                             error = function(e) NULL)
    if (is.null(yrs) || length(yrs)!=2) {
      failures <<- add_row(failures,
                           zip   = fname,
                           stage = "year-parse",
                           error = "cannot split year-range")
      return(NULL)
    }
    start_year   <- as.integer(yrs[1])
    end_year     <- as.integer(yrs[2])
    
    # list contents
    contents <- tryCatch(
      unzip(zf, list = TRUE),
      error = function(e) {
        failures <<- add_row(failures,
                             zip   = fname,
                             stage = "list-contents",
                             error = e$message)
        NULL
      }
    )
    if (is.null(contents)) return(NULL)
    
    csvs <- contents$Name[grepl("\\.csv$", contents$Name)]
    if (length(csvs)==0) {
      failures <<- add_row(failures,
                           zip   = fname,
                           stage = "filter-csv",
                           error = "no .csv entries in archive")
      return(NULL)
    }
    
    # extract CSVs
    extracted <- tryCatch(
      unzip(zf, files = csvs, exdir = exdir, overwrite = overwrite),
      error = function(e) {
        failures <<- add_row(failures,
                             zip   = fname,
                             stage = "extract",
                             error = e$message)
        character(0)
      }
    )
    if (length(extracted)==0) return(NULL)
    
    # one row per CSV
    tibble(
      file         = file.path(exdir, extracted),
      data_center  = data_center,
      site         = site,
      data_product = data_product,
      dataset      = dataset,
      start_year   = start_year,
      end_year     = end_year
    )
  })
  
  list(
    successes = successes,
    failures  = failures
  )
}




#########
######### discover_AMF_files
######### to find out how many AMF fluxnet files are available 
# ------------------------
# discover_AMF_files()
# ------------------------
# Scans for AmeriFlux “FLUXNET2015” FULLSET & SUBSET files,
# then builds a manifest with columns:
#   path, data_center, site, data_product, dataset, time_integral, start_year, end_year
discover_AMF_files <- function(data_dir = "data") {
library(fs); library(stringr); library(dplyr)
library(tidyr); library(tibble)

all_csvs <- fs::dir_ls(data_dir, recurse = TRUE, regexp = "\\.csv$")

# 1) Standard FLUXNET files (including HR)
std_pat <- 
  "^AMF_([^_]+)_FLUXNET_(FULLSET|SUBSET|ERA5)_(YY|DD|HH|MM|WW|HR)_([0-9]{4})-([0-9]{4})_.*\\.csv$"
std <- tibble(path = all_csvs) %>%
  filter(str_detect(basename(path), std_pat)) %>%
  mutate(filename = basename(path)) %>%
  tidyr::extract(                              # <— qualify here
    filename,
    into  = c("site","dataset","time_integral","start_year","end_year"),
    regex = std_pat, remove = FALSE, convert = TRUE
  ) %>%
  mutate(
    data_center  = "AMF",
    data_product = "FLUXNET"
  ) %>%
  select(path, filename, data_center, site, data_product,
         dataset, time_integral, start_year, end_year)

# 2) AUXMETEO / AUXNEE (no separate time_integral)
aux_pat <-
  "^AMF_([^_]+)_FLUXNET_(AUXMETEO|AUXNEE)_([0-9]{4})-([0-9]{4})_.*\\.csv$"

aux <- tibble(path = all_csvs) %>%
  dplyr::filter(stringr::str_detect(basename(path), aux_pat)) %>%
  dplyr::mutate(filename = basename(path)) %>%
  tidyr::extract(                              # <-- qualify this one too
    filename,
    into  = c("site","dataset","start_year","end_year"),
    regex = aux_pat, remove = FALSE, convert = TRUE
  ) %>%
  dplyr::mutate(
    data_center   = "AMF",
    data_product  = "FLUXNET",
    time_integral = NA_character_
  ) %>%
  dplyr::select(path, filename, data_center, site, data_product,
                dataset, time_integral, start_year, end_year)

# 3) Now coerce types and stitch together
make_types <- function(df) {
  df %>% mutate(
    across(c(data_center, site, data_product, dataset, time_integral),
           as.character),
    across(c(start_year, end_year), as.integer)
  )
}
manifest <- bind_rows(make_types(std), make_types(aux))

# 4) Summarize
manifest %>%
  group_by(time_integral = coalesce(time_integral, "AUX"), dataset) %>%
  summarise(
    unique_sites     = n_distinct(site),
    total_site_years = sum(end_year - start_year + 1, na.rm=TRUE),
    n_files          = n(),
    .groups          = "drop"
  ) %>%
  arrange(time_integral, dataset) %>%
  rowwise() %>%
  do({
    cat(sprintf("• %s / %s → %d sites, %d site-years across %d files\n",
                .$time_integral, .$dataset,
                .$unique_sites,   .$total_site_years, .$n_files))
    tibble()
  })

invisible(manifest)
}


#####Discover ICOS files
discover_ICOS_files <- function(data_dir = "data") {
  library(fs)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(tibble)
  
  # 1. Grab every CSV under data_dir
  all_csvs <- fs::dir_ls(data_dir, recurse = TRUE, regexp = "\\.csv$")
  
  # 2a. Legacy FLX_FULLSET / SUBSET files
  flx_manifest <- tibble(path = all_csvs) %>%
    dplyr::filter(stringr::str_detect(basename(path),
                                      "^FLX_[^_]+_FLUXNET2015_(FULLSET|SUBSET)_(YY|DD|HH|WW)_[0-9]{4}-[0-9]{4}_.*\\.csv$"
    )) %>%
    dplyr::mutate(filename = basename(path)) %>%
    tidyr::extract(                      # <-- qualify
      col   = "filename",
      into  = c("data_center","site","data_product",
                "dataset","time_integral","start_year","end_year"),
      regex = "^(FLX)_([^_]+)_(FLUXNET2015)_(FULLSET|SUBSET)_(DD|HH|MM|WW|YY_INTERIM|YY|AUXNEE|AUXMETEO)_([0-9]{4})-([0-9]{4})_.*\\.csv$",
      remove = FALSE, convert = TRUE
    ) %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    dplyr::mutate(across(ends_with("year"), as.integer)) %>%
    dplyr::select(path, filename, data_center, site, data_product,
                  dataset, time_integral, start_year, end_year)
  
  # 2b. Modern ICOSETC L2 files
  icos_manifest <- tibble(path = all_csvs) %>%
    dplyr::filter(stringr::str_detect(basename(path), "^ICOSETC_[^_]+_FLUXNET")) %>%
    dplyr::mutate(filename = basename(path)) %>%
    tidyr::extract(                      # <-- qualify
      col   = "filename",
      into  = c("data_center","site","data_product","time_integral","dataset"),
      regex = "^(ICOSETC)_([^_]+)_(FLUXNET)_(DD|HH|MM|WW|YY_INTERIM|YY|AUXNEE|AUXMETEO)_(L2)\\.csv$",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      start_year = NA_integer_,
      end_year   = NA_integer_
    ) %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    dplyr::mutate(across(ends_with("year"), as.integer)) %>%
    dplyr::select(path, filename, data_center, site, data_product,
                  dataset, time_integral, start_year, end_year)
  
  # 3. Combine and enforce the same columns as AMF
  manifest <- bind_rows(flx_manifest, icos_manifest)
  
  # 4. Print the same style of summary as discover_AMF_files()
  manifest %>%
    group_by(time_integral, dataset) %>%
    summarise(
      unique_sites     = n_distinct(site),
      total_site_years = sum(end_year - start_year + 1, na.rm = TRUE),
      n_files          = n(),
      .groups          = "drop"
    ) %>%
    arrange(time_integral, dataset) %>%
    rowwise() %>%
    do({
      cat(sprintf(
        "• %s / %s → %d sites, %d total site-years across %d files\n",
        .$time_integral, .$dataset,
        .$unique_sites,   .$total_site_years, .$n_files
      ))
      tibble()
    })
  
  invisible(manifest)
}

######
# function, organize_fluxnet_files(), which:
#   Takes any manifest-like tibble with columns
# – path (full file path)
# – time_integral (e.g. "DD", "AUXMETEO", etc.)
# – dataset (e.g. "L2", "FULLSET", etc.)
# A base output directory
# Reproduces exactly the same logic you had:
#   Builds dest_dir = base / {time_integral}_{dataset}, or the base itself if either is NA
# Creates all needed folders
# Moves files, overwriting only when the source is strictly newer
# use for AMF
# organize_fluxnet_files(manifest = AMF_manifest_new, base_out = file.path("data", "FLUXNET", "AMF"))
# use for ICOS
# organize_fluxnet_files(manifest = icos_manifest_new,base_out = file.path("data", "FLUXNET", "ICOS"))
# 

######
library(fs)
library(dplyr)

organize_fluxnet_files <- function(manifest, base_out) {
  manifest2 <- manifest %>%
    mutate(
      dest_dir = case_when(
        # AUXMETEO/AUXNEE → put them under base_out/AUXMETEO or AUXNEE
        is.na(time_integral) & !is.na(dataset) ~ file.path(base_out, dataset),
        # everything else → time_integral_dataset
        !is.na(time_integral) & !is.na(dataset) ~
          file.path(base_out, paste0(time_integral, "_", dataset)),
        # fallback → just the base
        TRUE ~ base_out
      )
    )
  
  # create folders
  unique(manifest2$dest_dir) %>%
    walk(~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  
  # move with overwrite‐newer logic (as before)…
  manifest2 %>%
    rowwise() %>%
    do({
      src  <- .$path
      dest <- file.path(.$dest_dir, basename(src))
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      
      if (src != dest) {
        src_mtime <- file.info(src)$mtime
        
        should_move <- if (file.exists(dest)) {
          dest_mtime <- file.info(dest)$mtime
          src_mtime > dest_mtime
        } else {
          TRUE
        }
        
        if (should_move) {
          file.copy(src, dest, overwrite = TRUE)
          file.remove(src)
        }
      }
      tibble()
    }) %>% invisible()
  
  message("All files have been reorganized under ", base_out)
  invisible(manifest2)
}

#######
#######
# Filter manifest

# ----------------------------------------------------------------------------
# Manifest filtering (no name collisions)
# ----------------------------------------------------------------------------
filter_manifest <- function(manifest,
                            data_center    = NULL,
                            flux_product   = NULL,
                            dataset_type   = NULL,
                            time_integrals = NULL,
                            sites          = NULL) {
  mf <- manifest
  if (!is.null(data_center))    mf <- filter(mf, .data$data_center   %in% data_center)
  if (!is.null(flux_product))   mf <- filter(mf, .data$data_product   %in% flux_product)
  if (!is.null(dataset_type))   mf <- filter(mf, .data$dataset        %in% dataset_type)
  if (!is.null(time_integrals)) mf <- filter(mf, .data$time_integral  %in% time_integrals)
  if (!is.null(sites))          mf <- filter(mf, .data$site           %in% sites)
  mf
}

# general load_fluxnet_data function
# Load & cache with manifest
# 
# USAGE -Load all AmeriFlux Annual data:
# 1) Load & cache all AMF FULLSET annual data:
# Annual_allAMF_FULLSET <- load_fluxnet_data2(
#   manifest      = all_manifest,
#   cache_file    = "cache/annual_allAMF_FULLSET.rds",
#   data_center   = "AMF",
#   dataset_type  = "FULLSET",
#   time_integrals= "YY"
# )


# ----------------------------------------------------------------------------
# Loader + cache
# ----------------------------------------------------------------------------
load_fluxnet_data <- function(manifest,
                              cache_file     = NULL,
                              data_center    = NULL,
                              flux_product   = NULL,
                              dataset_type   = NULL,
                              time_integrals = NULL,
                              sites          = NULL,
                              reader         = readr::read_csv,
                              reader_args    = list(show_col_types = FALSE)) {
  # Subset the manifest based on the filters provided
  mf <- filter_manifest(
    manifest,
    data_center    = data_center,
    flux_product   = flux_product,
    dataset_type   = dataset_type,
    time_integrals = time_integrals,
    sites          = sites
  )
  
  message(
    "→ Will read ", nrow(mf), " file(s) with integrals: ",
    paste(unique(mf$time_integral), collapse = ", "),
    "  (sites: ", if (is.null(sites)) "ALL" else paste(sites, collapse = ","), ")"
  )
  
  # If nothing to read, return an empty tibble with a helpful shape
  if (nrow(mf) == 0) {
    return(tibble::tibble())
  }
  
  paths <- mf$path
  
  # --------
  # NO CACHE MODE: cache_file is NULL → read everything and return
  # --------
  if (is.null(cache_file)) {
    out <- purrr::map_dfr(paths, function(p) {
      do.call(reader, c(list(file = p), reader_args)) |>
        dplyr::mutate(path = p, .before = 1)
    }) |>
      dplyr::left_join(
        dplyr::select(
          mf, path, data_center, data_product, dataset, time_integral,
          start_year, end_year, site
        ),
        by = "path"
      )
    
    return(out)
  }
  
  # --------
  # CACHE MODE: incremental read + saveRDS
  # --------
  # Ensure the cache directory exists
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # Load existing cache or start fresh
  if (file.exists(cache_file)) {
    data      <- readRDS(cache_file)
    seen      <- unique(data$path)
    new_paths <- setdiff(paths, seen)
  } else {
    data      <- tibble::tibble()
    new_paths <- paths
  }
  
  # Read any new files and append to cache
  if (length(new_paths) > 0) {
    message("Reading ", length(new_paths), " new file(s) …")
    new_data <- purrr::map_dfr(new_paths, function(p) {
      do.call(reader, c(list(file = p), reader_args)) |>
        dplyr::mutate(path = p, .before = 1) |>
        dplyr::left_join(
          dplyr::select(
            mf, path, data_center, data_product, dataset, time_integral,
            start_year, end_year, site
          ),
          by = "path"
        )
    })
    data <- dplyr::bind_rows(data, new_data)
    saveRDS(data, cache_file)
  }
  
  # Return only the rows that correspond to the current manifest subset
  data |>
    dplyr::semi_join(dplyr::select(mf, path), by = "path")
}


#simple clean fluxnet data function
clean_fluxnet_data <- function(data, site_metadata) {
  data %>%
    # 1) replace sentinels in all numeric columns
    mutate(across(where(is.numeric), ~ if_else(.x < -9000, NA_real_, .x))) %>%
    # 2) join your metadata by site
    left_join(site_metadata, by = c("site" = "SITE_ID"))
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
      COUNTRY,
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

#.QA QC filters
# helper that adds pct_gapfilled + is_bad, based on the YY QC column for the chosen var
#

flag_bad_gapfilled <- function(df, gate_var, max_gapfilled = 0.5, drop_if_missing = TRUE) {
  qc_col <- paste0(gate_var, "_QC")
  if (!qc_col %in% names(df)) {
    warning(sprintf("QC column `%s` not found. No filtering applied.", qc_col))
    df$pct_gapfilled <- NA_real_
    df$is_bad <- if (drop_if_missing) TRUE else FALSE
    return(df)
  }
  pg <- df[[qc_col]]                        # fraction "good" at YY (0..1)
  df$pct_gapfilled <- pmax(0, pmin(1, 1 - pg))  # clamp just in case
  df$is_bad <- ifelse(
    is.na(df$pct_gapfilled),
    drop_if_missing,                        # drop if we can't assess quality
    df$pct_gapfilled > max_gapfilled
  )
  df
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
