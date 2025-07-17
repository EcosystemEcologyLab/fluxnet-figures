# Unzip just the files you need

# There is an API that allows programmatic download of data
# (https://github.com/ICOS-Carbon-Portal/data?tab=readme-ov-file#downloading-originals-programmatically).
# There is also an R package interface to the "carbon portal"
# (https://github.com/bluegreen-labs/icoscp).  This script assumes you've
# downloaded a zip archive of all the data from this URL:
# https://github.com/bluegreen-labs/icoscp
library(fs)
library(dplyr)
library(stringr)

big_zip <- "data/Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1.zip"
site_zips <- unzip(big_zip, list = TRUE) |>
   mutate(country_site = str_extract(Name, "^ICOSETC_([a-zA-Z-]+)", group = 1)) |> 
   separate(country_site, into = c("country", "site"), sep = "-")

site_zips <- site_zips |>
  # looks like we only care about the ARCHIVE zip (and only some files in it)
 filter(str_detect(Name, "ARCHIVE")) #|> 
  #possibly also filter by country or something
  #  filter(country == "FR", site == "Bil")


#what CSVs do we actually need?  
# - SITEINFO csv from ICOS ARCHIVE zip
# - FLUXNET_DD_L2 csv from ICOS ARCHIVE zip

site_zip_paths <- unzip(big_zip, files = site_zips$Name, exdir = path("data", "ICOS"))

# transform the zip path into the SITEINFO and FLUXNET_DD_L2 csv paths
# E.g. ICOSETC_UK-AMo_ARCHIVE_L2.zip contains ICOSETC_UK-AMo_SITEINFO_L2.csv and ICOSETC_UK-AMo_FLUXNET_DD_L2.csv
# site_zip_path <- site_zip_paths[1]
walk(site_zip_paths, function(site_zip_path) {
  siteinfo_csv <- site_zip_path |>
    path_file() |>
    path_ext_remove() |>
    str_replace("ARCHIVE", "SITEINFO") |>
    path_ext_set("csv")
  dd_csv <- site_zip_path |>
    path_file() |>
    path_ext_remove() |>
    str_replace("ARCHIVE", "FLUXNET_DD") |>
    path_ext_set("csv")

  unzip(
    site_zip_path,
    files = c(siteinfo_csv, dd_csv),
    exdir = path_ext_remove(site_zip_path)
  )
})
