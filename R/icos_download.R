library(httr2)
#' Download full zip of ICOS data from https://www.icos-cp.eu/data-products/ecosystem-release
#' 
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
    httr2::req_url_query(ids = '[\"1-HA2r4l5QUjAgQr5CCEfJe3\"]', isColl = "true") |> 
    httr2::req_cookies_set() |> 
    httr2::req_progress(type = "down") # FIXME: this doesn't work.  Report bug?

  # Save result to tempflie for now, then rename using filename data stored in
  # response headers
  tmp <- withr::local_tempfile()
  resp <- httr2::req_perform(req, path = tmp)
  content <- httr2::resp_header(resp, header = "content-disposition")
  filename <- stringr::str_extract(content, 'attachment; filename=\"(.+)\"', group = 1)
  out <- fs::file_move(tmp, path(dir, filename))
  
  #return file path:
  out
}

extract_icos <- function(
  dir = "data/ICOS",
  site_id = NULL,
  period = c("YY", "MM", "WW", "DD", "HH")
) {}

#' Read in ICOS CSVs after extracting
read_icos <- function(
  dir = "data/ICOS",
  site_id = NULL,
  period = c("YY", "MM", "WW", "DD", "HH")

){}