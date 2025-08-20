# Download all the data programmatically

# new ICOS data
source("R/icos_download_extract_read.R")
icos_download()
icos_extract(
  zip = "data/Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1.zip",
  outdir = "data/ICOS",
  period = c("YY", "DD")
)

# fluxnet (including legacy ICOS?)
library(amerifluxr)
sites <- amf_sites()
fluxnet_files <- tibble(
  path = dir_ls(
    "data/",
    regexp = "AMF_[^_]+_FLUXNET_FULLSET_(?:YY|DD|HH|WW)_[0-9]{4}-[0-9]{4}_.*\\.csv$",
    recurse = TRUE
  )
) |>
  mutate(
    filename = path_file(path),
    to_split = path_ext_remove(filename)
  ) |>
  separate_wider_delim(
    to_split,
    names = c(
      "source",
      "SITE_ID",
      "fluxnet",
      "set",
      "period",
      "yr_range",
      "junk"
    ),
    delim = "_"
  ) |>
  select(-fluxnet, -junk) |>
  separate_wider_delim(
    SITE_ID,
    names = c("country_code", "site"),
    delim = "-",
    cols_remove = FALSE
  )

sites_to_dl <- setdiff(sites$SITE_ID, unique(fluxnet_files$SITE_ID))

zip_paths <- amf_download_fluxnet(
  user_id = "Aariq",
  user_email = "scottericr@gmail.com",
  site_id = sites_to_dl,
  data_variant = "FULLSET",
  data_policy = "CCBY4.0",
  agree_policy = TRUE,
  intended_use = "synthesis",
  intended_use_text = "creating pipeline for standardized figures",
  out_dir = "data/AMF"
)

# Only unzip files needed
walk(
  zip_paths,
  \(zip_path) {
    t <- c("DD", "YY") #which FULLSET CSVs to extract
    exdir <- fs::path_ext_remove(zip_path)
    #split apart the file name and insert `t` in the right place
    file_vect <- zip_path |>
      path_file() |>
      stringr::str_split("_") |>
      pluck(1)
    files <- paste(
      paste(file_vect[1:4], collapse = "_"),
      t,
      paste(file_vect[5:6], collapse = "_"),
      sep = "_"
    ) |>
      fs::path_ext_set("csv")

    unzip(zip_path, files = files, exdir = exdir)
  },
  .progress = TRUE
)
