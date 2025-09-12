# ------------------------------------------------------------
# Merge global network site lists into a single master table
# Robust to column-name variations across exports
# ------------------------------------------------------------

# install.packages(c("readr","dplyr","stringr","janitor","purrr"))
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(purrr)

# ---- 0) Config ----
data_dir <- "data/FLUXNET"   # <- change to your folder
fp <- function(x) file.path(data_dir, x)

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9\\-\\.]+", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

first_existing_name <- function(df, candidates) {
  nm <- intersect(candidates, names(df))
  if (length(nm) == 0) NA_character_ else nm[1]
}

# ---- 1A) AsiaFlux + JapanFLUX (combined list you generated) ----
asia_japan <- read_csv(fp("AsiaFlux_JapanFlux_2025_with_country_matchcheck.csv"),
                       locale = locale(encoding = "UTF-8"),
                       guess_max = 10000) |>
  clean_names() |>
  transmute(
    network = case_when(
      !is.na(source) & str_detect(tolower(source), "japan") ~ "JapanFLUX",
      !is.na(source) & str_detect(tolower(source), "asia")  ~ "AsiaFlux",
      TRUE ~ coalesce(source, "Asia/JapanFlux")
    ),
    source_file = "AsiaFlux_JapanFlux_2025_with_country_matchcheck.csv",
    site_code   = coalesce(site, site_original, NA_character_),
    site_name   = coalesce(site_name, NA_character_),
    country     = coalesce(country, NA_character_),
    latitude    = to_num(latitude),
    longitude   = to_num(longitude),
    igbp        = coalesce(igbp, NA_character_),
    landcover   = NA_character_,
    start_year  = NA_integer_,
    end_year    = NA_integer_,
    status      = NA_character_,
    active      = NA
  )

# ---- 1B) West Africa custom list ----
waf <- read_csv(fp("west_africa_sites.csv"),
                locale = locale(encoding = "UTF-8"),
                guess_max = 10000) |>
  clean_names() |>
  transmute(
    network     = "WestAfrica",
    source_file = "west_africa_sites.csv",
    site_code   = NA_character_,
    site_name   = coalesce(site, NA_character_),
    country     = coalesce(country, NA_character_),
    latitude    = to_num(latitude),
    longitude   = to_num(longitude),
    igbp        = NA_character_,
    landcover   = NA_character_,
    start_year  = NA_integer_,
    end_year    = NA_integer_,
    status      = NA_character_,
    active      = NA
  )

# ---- 1C) FLUXNET2015 ----
fluxnet2015 <- read_csv(fp("fluxnet2015_sites_web.csv"),
                        locale = locale(encoding = "latin1"),
                        guess_max = 10000) |>
  clean_names() |>
  transmute(
    network     = "FLUXNET2015",
    source_file = "fluxnet2015_sites_web.csv",
    site_code   = coalesce(site_id, NA_character_),
    site_name   = coalesce(site_name, NA_character_),
    country     = NA_character_,
    latitude    = to_num(location_lat),
    longitude   = to_num(location_long),
    igbp        = coalesce(igbp, NA_character_),
    landcover   = NA_character_,
    start_year  = NA_integer_,
    end_year    = NA_integer_,
    status      = "Listed",
    active      = NA
  )

# ---- 1D) OzFlux 2025 (lat/long only) ----
ozflux_raw <- read_csv(fp("ozflux_sites_2025.csv"),
                       locale = locale(encoding = "UTF-8"),
                       guess_max = 10000) |>
  clean_names()

ozflux <- ozflux_raw |>
  transmute(
    network     = "OzFlux",
    source_file = "ozflux_sites_2025.csv",
    site_code   = coalesce(fluxnet, NA_character_),  # AU-xxx when present
    site_name   = coalesce(name, NA_character_),
    country     = "Australia",
    latitude    = to_num(lat),
    longitude   = to_num(long),
    igbp        = NA_character_,
    landcover   = coalesce(landcover, NA_character_),
    start_year  = NA_integer_,
    end_year    = NA_integer_,
    status      = coalesce(status, NA_character_),
    active      = case_when(
      !is.na(active) & str_detect(tolower(active), "y|true|active") ~ TRUE,
      !is.na(active) & str_detect(tolower(active), "n|false|inactive") ~ FALSE,
      TRUE ~ NA
    )
  )

# ---- 1E) ICOS ----
icos <- read_csv(fp("ICOS_allProjects_SitesList.csv"),
                 locale = locale(encoding = "UTF-8"),
                 guess_max = 10000) |>
  clean_names() |>
  transmute(
    network     = "ICOS",
    source_file = "ICOS_allProjects_SitesList.csv",
    site_code   = coalesce(site_code, NA_character_),
    site_name   = coalesce(site_name, NA_character_),
    country     = NA_character_,
    latitude    = to_num(site_latitude),
    longitude   = to_num(site_longitude),
    igbp        = coalesce(igbp_code, NA_character_),
    landcover   = NA_character_,
    start_year  = NA_integer_,
    end_year    = NA_integer_,
    status      = NA_character_,
    active      = NA
  )

# ---- 1F) AmeriFlux: All FLUXNET (robust start/end mapping) ----
amf_raw <- read_csv(fp("AmeriFlux-AllFLUXNET.csv"),
                    locale = locale(encoding = "UTF-8"),
                    guess_max = 10000) |>
  clean_names()

start_var <- first_existing_name(
  amf_raw,
  c("ameriflux_fluxnet_data_start", "site_start", "ameriflux_base_data_start")
)
end_var <- first_existing_name(
  amf_raw,
  c("ameriflux_fluxnet_data_end", "site_end", "ameriflux_base_data_end")
)

amf_fluxnet <- amf_raw |>
  transmute(
    network     = "AmeriFlux (FLUXNET registry)",
    source_file = "AmeriFlux-AllFLUXNET.csv",
    site_code   = coalesce(site_id, NA_character_),
    site_name   = coalesce(name, NA_character_),
    country     = coalesce(country, NA_character_),
    latitude    = to_num(`latitude_degrees`),
    longitude   = to_num(`longitude_degrees`),
    igbp        = `vegetation_abbreviation_igbp`,
    landcover   = `vegetation_description_igbp`,
    start_year  = suppressWarnings(as.integer(
      if (is.na(start_var)) NA_real_ else .data[[start_var]]
    )),
    end_year    = suppressWarnings(as.integer(
      if (is.na(end_var)) NA_real_ else .data[[end_var]]
    )),
    status      = "Listed",
    active      = NA
  )

# ---- 1G) AmeriFlux: All Sites With Data (robust) ----
amf2_raw <- read_csv(fp("AmeriFlux-AllSitesWithData.csv"),
                     locale = locale(encoding = "UTF-8"),
                     guess_max = 10000) |>
  clean_names()

start_var2 <- first_existing_name(
  amf2_raw,
  c("site_start", "ameriflux_fluxnet_data_start", "ameriflux_base_data_start")
)
end_var2 <- first_existing_name(
  amf2_raw,
  c("site_end", "ameriflux_fluxnet_data_end", "ameriflux_base_data_end")
)

amf_with_data <- amf2_raw |>
  transmute(
    network     = "AmeriFlux",
    source_file = "AmeriFlux-AllSitesWithData.csv",
    site_code   = coalesce(site_id, NA_character_),
    site_name   = coalesce(name, NA_character_),
    country     = coalesce(country, NA_character_),
    latitude    = to_num(`latitude_degrees`),
    longitude   = to_num(`longitude_degrees`),
    igbp        = `vegetation_abbreviation_igbp`,
    landcover   = `vegetation_description_igbp`,
    start_year  = suppressWarnings(as.integer(
      if (is.na(start_var2)) NA_real_ else .data[[start_var2]]
    )),
    end_year    = suppressWarnings(as.integer(
      if (is.na(end_var2)) NA_real_ else .data[[end_var2]]
    )),
    status      = "Has Data",
    active      = NA
  )

# ---- 2) Bind & clean ----
sites_master <- bind_rows(
  asia_japan, waf, fluxnet2015, ozflux, icos, amf_fluxnet, amf_with_data
) |>
  mutate(
    across(c(site_code, site_name, country, igbp, landcover, status),
           ~ ifelse(is.character(.x), str_squish(.x), .x)),
    latitude  = to_num(latitude),
    longitude = to_num(longitude)
  ) |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  distinct(network, site_code, site_name, latitude, longitude, .keep_all = TRUE)

# ---- 3) Save + quick checks ----
out_path <- fp("EddyFluxsites_master_merged.csv")
write_csv(sites_master, out_path)
message("Wrote: ", normalizePath(out_path))

sites_master |>
  count(network, sort = TRUE) |>
  print(n = Inf)

summary(dplyr::select(sites_master, latitude, longitude))


