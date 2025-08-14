#### NOTE: I hate to have parallel code rather than fixing the functions you've 
# already written, but I can't seem to figure out where ICOS files that start 
# with "FLX" came from

library(httr2)
#' Download full zip of ICOS data from https://www.icos-cp.eu/data-products/ecosystem-release
#' 
#' @author Eric R. Scott
#' @param dir where to download this big zip file
icos_download <- function(dir = "data") {
  cli::cli_inform(c(
    "!" = "You are required to accept the license agreement to download:",
    "i" = "I hereby confirm that I have taken notice of the information provided to inform me about the data and good practices of data usage. These guidelines do not define additional contractual conditions."
  ))
  choice <- menu(choices = c("yes", "no"), title = "Do you accept?")
  if (choice != 1) {
    stop("You must accept the license agreement to download!")
  }

  # This request accepts the license and automatically redirects to the download
  req_icos <- httr2::request("https://data.icos-cp.eu/")
  req <- req_icos |>
    httr2::req_url_path_append("licence_accept") |>
    httr2::req_url_query(
      ids = '[\"1-HA2r4l5QUjAgQr5CCEfJe3\"]',
      isColl = "true"
    ) |>
    httr2::req_cookies_set() |>
    httr2::req_progress(type = "down") # FIXME: this doesn't work.  Report bug?

  # Save result to tempflie for now, then rename using filename data stored in
  # response headers
  tmp <- withr::local_tempfile()
  resp <- httr2::req_perform(req, path = tmp)
  content <- httr2::resp_header(resp, header = "content-disposition")
  filename <- stringr::str_extract(
    content,
    'attachment; filename=\"(.+)\"',
    group = 1
  )
  out <- fs::file_move(tmp, path(dir, filename))

  #return file path:
  out
}

#' Extract ICOS CSVs
#'
#' Happens in two stages: 1) extract specificed (or all) site zip files from
#' giant 10GB zip file, 2) extract specific files from site zip files.
#'
#' NOTE: for now this only extracts zip files with "ARCHIVE" in the filename and
#' from that only CSVs with "FLUXNET" (but not "VARINFO_FLUXNET") or "SITEINFO"
#' in the name.
#' 
#' @author Eric R. Scott
#'
#' @param zip the big 10GB zip file created by `icos_download()`
#' @param outdir where to put individual site zip files
#' @param site_ids an optional vector of site IDs to extract. `site_ids` are a
#'   2-letter country code and a 3-letter site code.  If `NULL` (default), all
#'   site zip files will be extracted.
#' @param period which FLUXNET CSVs to extract. Multiple choices allowed.
#' 
icos_extract <- function(
  zip,
  outdir = "data/ICOS",
  site_ids = NULL,
  # countries = NULL, #TODO
  period = c("YY", "MM", "WW", "DD", "HH")
) {
  period <- match.arg(period, several.ok = TRUE)
  site_zips <- unzip(zip, list = TRUE) |>
    dplyr::mutate(
      site_id = stringr::str_extract(
        Name,
        "^ICOSETC_([a-zA-Z-]+)",
        group = 1
      )
    ) |>
    tidyr::separate_wider_delim(
      site_id,
      names = c("country", "site"),
      delim = "-",
      cols_remove = FALSE
    ) |>
    # looks like we only care about the ARCHIVE zip (and only some files in it)
    dplyr::filter(stringr::str_detect(Name, "ARCHIVE"))

  if (!is.null(site_ids)) {
    site_zips <- site_zips |> dplyr::filter(site_id %in% site_ids)
  }
  site_zip_paths <- unzip(zip, files = site_zips$Name, exdir = outdir)

  # transform the zip path into the SITEINFO and FLUXNET_<period>_L2 csv paths
  # E.g. ICOSETC_UK-AMo_ARCHIVE_L2.zip contains ICOSETC_UK-AMo_SITEINFO_L2.csv
  # and ICOSETC_UK-AMo_FLUXNET_DD_L2.csv

  # site_zip_path <- site_zip_paths[1]

  purrr::walk(site_zip_paths, function(site_zip_path) {
    siteinfo_csv <-
      site_zip_path |>
      fs::path_file() |>
      str_replace("ARCHIVE", "SITEINFO") |>
      fs::path_ext_set("csv")

    fluxnet_csvs <-
      site_zip_path |>
      fs::path_file() |>
      str_replace("ARCHIVE", glue::glue("FLUXNET_{period}")) |>
      fs::path_ext_set("csv")

    # TODO: would it be better to put all the daily in one folder, all the
    # yearly in another, etc?
    unzip(
      site_zip_path,
      files = c(siteinfo_csv, fluxnet_csvs),
      exdir = fs::path_ext_remove(site_zip_path)
    )
  })
}

#' Read in ICOS FLUXNET CSVs after extracting
#' 
#' @author Eric R. Scott
#'
#' @param dir directory to look for CSVs in.  Will look recursively, so folders
#'   nested under this are OK.
#' @param site_id an optional vector of site IDs to read in. `site_ids` are a
#'   2-letter country code and a 3-letter site code.  If `NULL` (default), all
#'   sites found will be used.
#' @param period choose which period to read in. Only one choice is allowed
#' 
icos_read_fluxnet <- function(
  dir = "data/ICOS",
  period = c("YY", "MM", "WW", "DD", "HH"),
  site_ids = NULL,
  manifest = NULL #TODO: optionally provide manifest with a "filepath" column?
) {
  sel_period = match.arg(period)
  all_csvs <- fs::dir_ls(dir, recurse = TRUE, glob = "*.csv")

  icos_csvs <- dplyr::tibble(path = all_csvs) |>
    dplyr::mutate(file = fs::path_file(path)) |>
    #just in case dir is polluted with other CSVs
    filter(str_detect(file, "^ICOSETC")) |>
    filter(str_detect(file, "(?<!VARINFO)_FLUXNET_")) |>
    tidyr::separate_wider_delim(
      cols = file,
      delim = "_",
      names = c("ICOSETC", "site_id", "FLUXNET", "period", "L2.csv")
    ) |>
    select(-ICOSETC, -FLUXNET, -L2.csv) |>
    filter(period %in% sel_period)

  if (!is.null(site_ids)) {
    icos_csvs <- icos_csvs |> filter(site_id %in% site_ids)
  }

  # Can't do this because they all have different column numbers
  df <- purrr::map(icos_csvs$path, readr::read_csv) |>
    purrr::set_names(icos_csvs$site_id) |>
    purrr::list_rbind(names_to = "site_id")

  # basic data cleaning
  df <- df |>
    mutate(across(where(is.numeric), \(x) na_if(x, -9999)))

  if (period == "YY") {
    df <- df |>
      mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP)
  }

  if (period == "MM") {
    df <- df |>
      mutate(
        yearmonth = tsibble::yearmonth(
          as.character(TIMESTAMP),
          format = "%Y%m"
        ),
        .before = TIMESTAMP
      )
  }

  if (period == "WW") {
    df <- df |>
      mutate(
        date_start = lubridate::ymd(TIMESTAMP_START),
        date_end = lubridate::ymd(TIMESTAMP_END),
        .before = TIMESTAMP_START
      )
  }

  if (period %in% c("DD")) {
    df <- df |>
      mutate(date = lubridate::ymd(TIMESTAMP), .before = TIMESTAMP)
  }

  # return:
  df
}