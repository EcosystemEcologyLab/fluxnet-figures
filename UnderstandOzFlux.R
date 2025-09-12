# --- user paths (edit if needed) ---
base_dir      <- "data/FLUXNET/OZFLUX"
site_longname <- "AdelaideRiver"
version_dir   <- "2024_v2"
# -----------------------------------

site_dir <- file.path(base_dir, site_longname, version_dir)
stopifnot(dir.exists(site_dir))

# Match: contains "L6" AND ends with "_Annual.nc"
annual_pat <- "L6.*_Annual\\.nc$"
nc_path <- list.files(site_dir, pattern = annual_pat, full.names = TRUE)
if (length(nc_path) == 0) stop("No L6 *_Annual.nc file found in: ", site_dir)
if (length(nc_path) > 1) {
  # if multiple, pick the newest (you can change this rule)
  info <- file.info(nc_path); nc_path <- rownames(info)[which.max(info$mtime)]
  message("Multiple annual files. Using most recent: ", basename(nc_path))
} else {
  message("Using: ", basename(nc_path))
}


# install.packages(c("ncdf4","dplyr","purrr","stringr","tibble"))
library(ncdf4)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# heuristic mapper: OzFlux -> FLUXNET-like suggestions
suggest_fluxnet <- function(name, std_name, long_name, units) {
  n <- tolower(name)
  sn <- tolower(std_name %||% "")
  ln <- tolower(long_name %||% "")
  u  <- tolower(units %||% "")
  
  # helpers
  has <- function(pattern, x) str_detect(x, pattern)
  
  # Core fluxes
  if (n %in% c("fc","nee") || has("net ecosystem", ln) || has("co2_flux|surface_upward_co2", sn)) {
    return(list(fluxnet="NEE_VUT_REF", note="Check sign & units; VUT qualifier approximate."))
  }
  if (n %in% c("gpp") || has("gross primary", ln)) {
    return(list(fluxnet="GPP_NT_VUT_REF", note="Single GPP product; not split DT/NT."))
  }
  if (n %in% c("reco","er","re") || has("ecosystem respiration", ln)) {
    return(list(fluxnet="RECO_NT_VUT_REF", note="Single Reco; not split DT/NT."))
  }
  if (n == "le" || has("latent heat", ln) || has("surface_upward_latent_heat_flux", sn)) {
    return(list(fluxnet="LE_F_MDS", note="Units W m-2."))
  }
  if (n == "h" || has("sensible heat", ln) || has("surface_upward_sensible_heat_flux", sn)) {
    return(list(fluxnet="H_F_MDS", note="Units W m-2."))
  }
  
  # Radiation & light
  if (n %in% c("sw_in","rg") || has("shortwave", ln) || has("surface_downwelling_shortwave", sn)) {
    return(list(fluxnet="SW_IN", note="W m-2."))
  }
  if (n %in% c("ppfd","par") || has("photosynthetic photon", ln)) {
    return(list(fluxnet="PPFD_IN", note="µmol m-2 s-1."))
  }
  
  # Met
  if (n %in% c("ta","tair","air_temp") || has("air temperature", ln)) {
    return(list(fluxnet="TA", note="°C (convert if K)."))
  }
  if (n %in% c("vpd") || has("vpd", ln)) {
    return(list(fluxnet="VPD_F", note="Check units (hPa vs kPa)."))
  }
  if (n %in% c("rh")) {
    return(list(fluxnet="RH", note="%"))
  }
  if (n %in% c("pa","press","pressure") || has("air_pressure", sn)) {
    return(list(fluxnet="PA", note="kPa or hPa; confirm."))
  }
  if (n %in% c("p","precip","rain") || has("precipitation", ln)) {
    return(list(fluxnet="P_F", note="mm over interval; confirm aggregation."))
  }
  if (n %in% c("ws","ws_1","ws_2","wind_speed")) {
    return(list(fluxnet="WS", note="m s-1; check height."))
  }
  if (n %in% c("wd","wind_dir")) {
    return(list(fluxnet="WD", note="degrees."))
  }
  
  # Turbulence & storage
  if (n %in% c("ustar","u_star","u*")) {
    return(list(fluxnet="USTAR", note="m s-1."))
  }
  if (has("storage|stor|sco2", n) || has("tendency_of_atmosphere_mass_content_of_carbon_dioxide", sn)) {
    return(list(fluxnet="CO2_STOR", note="Check exact storage term name/units."))
  }
  
  # Soil
  if (n %in% c("ts","tsoil") || has("soil temperature", ln)) {
    return(list(fluxnet="TS_1", note="Map depth-specific variables to TS_* slots."))
  }
  if (n %in% c("swc","vwc","theta") || has("soil_moisture", sn) || has("soil water content", ln)) {
    return(list(fluxnet="SWC_1", note="Volumetric; map depths to SWC_* slots."))
  }
  
  # QC flags (generic)
  if (str_ends(n, "_qc")) {
    return(list(fluxnet=toupper(name), note="QC flag; map alongside primary var."))
  }
  
  # default: unknown → keep original, flag for review
  list(fluxnet=toupper(name), note="No direct map found; review.")
}

inspect_ozflux_annual <- function(nc_path) {
  nc <- nc_open(nc_path)
  
  # dimension summary
  dims <- tibble(
    dim = names(nc$dim),
    len = vapply(nc$dim, `[[`, integer(1), "len"),
    units = vapply(nc$dim, function(d) d$units %||% "", character(1))
  )
  
  # variable summary (+ mapping suggestion)
  vars <- names(nc$var)
  var_tbl <- tibble(
    var        = vars,
    long_name  = vapply(nc$var, \(v) v$longname %||% "", character(1)),
    units      = vapply(nc$var, \(v) v$units    %||% "", character(1)),
    dims       = vapply(nc$var, \(v) paste(v$dimids, collapse=","), character(1))
  )
  
  # try to read CF 'standard_name' attribute when present
  std_names <- map_chr(vars, ~ ncatt_get(nc, .x, "standard_name")$value %||% "")
  var_tbl$standard_name <- std_names
  
  # mapping suggestion
  map_suggest <- pmap(var_tbl[,c("var","standard_name","long_name","units")],
                      ~ suggest_fluxnet(..1, ..2, ..3, ..4))
  var_tbl$FLUXNET_suggest <- vapply(map_suggest, `[[`, character(1), "fluxnet")
  var_tbl$Notes           <- vapply(map_suggest, `[[`, character(1), "note")
  
  # global attributes (handy later)
  gatts <- names(nc$gatts)
  nc_close(nc)
  
  list(
    file = nc_path,
    dims = dims,
    variables = var_tbl %>% arrange(var),
    global_attributes = gatts
  )
}

# ---- run it on the chosen file ----
res <- inspect_ozflux_annual(nc_path)

cat("\nDimensions:\n"); print(res$dims, n=Inf)
cat("\nVariables (first 25):\n"); print(head(res$variables, 25))
cat("\nTotal variables:", nrow(res$variables), "\n")

# optional: view all variables nicely
# View(res$variables)  # if you're in RStudio



library(ncdf4)
library(lubridate)

nc <- nc_open(nc_path)

# read numeric time
t_num <- if ("time" %in% names(nc$var)) {
  ncvar_get(nc, "time")
} else {
  nc$dim$time$vals
}

# read units (CF-style: "<units> since YYYY-mm-dd HH:MM:SS")
t_units <- if ("time" %in% names(nc$var)) {
  ncatt_get(nc, "time", "units")$value
} else {
  nc$dim$time$units
}

# simple CF time decoder for days/hours/seconds since origin
cf_to_posix <- function(tvals, tunits) {
  m <- regexec("^\\s*([A-Za-z]+)\\s+since\\s+([0-9Tt:\\-\\s\\.]+)", tunits)
  parts <- regmatches(tunits, m)[[1]]
  if (length(parts) < 3) stop("Unrecognized time units: ", tunits)
  unit <- tolower(parts[2])
  origin <- ymd_hms(gsub("t"," ", parts[3], ignore.case = TRUE), tz = "UTC")
  mult <- switch(unit,
                 "seconds"=1, "second"=1, "sec"=1,
                 "minutes"=60, "minute"=60, "min"=60,
                 "hours"=3600, "hour"=3600, "hr"=3600,
                 "days"=86400, "day"=86400,
                 stop("Unsupported unit: ", unit)
  )
  origin + tvals * mult
}

time_posix <- cf_to_posix(t_num, t_units)
years <- year(time_posix)

cat("Time units:", t_units, "\n")
print(time_posix)
print(years)





# install.packages(c("ncdf4","dplyr","purrr","stringr","lubridate","tibble"))
library(ncdf4)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

# --- CF time decoder (days|hours|seconds since <origin>) ---
cf_time_to_posix <- function(nc, time_var = "time") {
  `%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
  
  # values
  tvals <- if (time_var %in% names(nc$var)) ncvar_get(nc, time_var) else nc$dim[[time_var]]$vals
  
  # units + calendar
  tunits <- if (time_var %in% names(nc$var)) {
    ncatt_get(nc, time_var, "units")$value
  } else {
    nc$dim[[time_var]]$units %||% ncatt_get(nc, time_var, "units")$value
  }
  cal <- if (time_var %in% names(nc$var)) {
    ncatt_get(nc, time_var, "calendar")$value %||% "standard"
  } else {
    ncatt_get(nc, time_var, "calendar")$value %||% "standard"
  }
  
  # parse "<unit> since <origin>"
  m <- regexec("^\\s*([A-Za-z]+)\\s+since\\s+(.+?)\\s*$", tunits)
  parts <- regmatches(tunits, m)[[1]]
  if (length(parts) < 3) {
    return(list(values=tvals, units=tunits, calendar=cal,
                posix=as.POSIXct(rep(NA_real_, length(tvals)), tz="UTC")))
  }
  unit <- tolower(parts[2])
  origin_raw <- gsub("t"," ", trimws(parts[3]), ignore.case = TRUE)
  
  # fractional seconds-aware
  origin <- as.POSIXct(strptime(origin_raw, "%Y-%m-%d %H:%M:%OS", tz="UTC"))
  if (is.na(origin)) origin <- as.POSIXct(strptime(origin_raw, "%Y-%m-%d %H:%M:%S", tz="UTC"))
  if (is.na(origin)) origin <- as.POSIXct(strptime(origin_raw, "%Y-%m-%d %H:%M",    tz="UTC"))
  if (is.na(origin)) origin <- as.POSIXct(strptime(origin_raw, "%Y-%m-%d",          tz="UTC"))
  
  mult <- switch(unit,
                 "seconds"=1, "second"=1, "sec"=1,
                 "minutes"=60, "minute"=60, "min"=60,
                 "hours"=3600, "hour"=3600, "hr"=3600,
                 "days"=86400, "day"=86400,
                 NA_real_
  )
  
  posix <- if (is.na(origin) || is.na(mult)) {
    as.POSIXct(rep(NA_real_, length(tvals)), tz="UTC")
  } else {
    origin + as.numeric(tvals) * mult
  }
  
  list(values = tvals, units = tunits, calendar = cal, posix = posix)
}


# --- Try multiple locations for latitude/longitude (dims, vars, or globals) ---
read_latlon <- function(nc) {
  # 1) scalar vars commonly used
  cand_vars <- c("latitude","longitude","lat","lon")
  get_num <- function(nm) if (nm %in% names(nc$var)) as.numeric(ncvar_get(nc, nm)) else NA_real_
  
  lat <- get_num("latitude")
  lon <- get_num("longitude")
  if (is.na(lat) || is.na(lon)) {
    lat <- if ("lat" %in% names(nc$var)) as.numeric(ncvar_get(nc, "lat")) else lat
    lon <- if ("lon" %in% names(nc$var)) as.numeric(ncvar_get(nc, "lon")) else lon
  }
  
  # 2) dims sometimes carry values for point datasets
  if ((is.na(lat) || is.na(lon)) && "latitude" %in% names(nc$dim)) lat <- nc$dim[["latitude"]]$vals %||% lat
  if ((is.na(lat) || is.na(lon)) && "longitude" %in% names(nc$dim)) lon <- nc$dim[["longitude"]]$vals %||% lon
  
  # 3) CF global attrs fallbacks
  if (is.na(lat)) lat <- as.numeric(nc$gatts$geospatial_lat_min %||% NA)
  if (is.na(lon)) lon <- as.numeric(nc$gatts$geospatial_lon_min %||% NA)
  
  c(lat = suppressWarnings(as.numeric(lat))[1], lon = suppressWarnings(as.numeric(lon))[1])
}

# --- Single-file metadata probe ---
read_nc_metadata <- function(path) {
  nc <- nc_open(path)
  
  # time
  has_time <- ("time" %in% names(nc$var)) || ("time" %in% names(nc$dim))
  tinfo <- if (has_time) cf_time_to_posix(nc, "time") else list(values=numeric(0), units=NA, posix=POSIXct(character()))
  years <- if (length(tinfo$posix)) lubridate::year(tinfo$posix) else integer(0)
  
  # lat/lon
  ll <- read_latlon(nc)
  
  # quick summaries
  out <- tibble(
    file           = path,
    n_time         = length(tinfo$values),
    time_units     = tinfo$units %||% NA_character_,
    time_start_utc = if (length(tinfo$posix)) min(tinfo$posix, na.rm=TRUE) else as.POSIXct(NA),
    time_end_utc   = if (length(tinfo$posix)) max(tinfo$posix, na.rm=TRUE) else as.POSIXct(NA),
    start     = if (length(years)) min(years, na.rm=TRUE) else NA_integer_,
    end      = if (length(years)) max(years, na.rm=TRUE) else NA_integer_,
    lat            = ll["lat"],
    lon            = ll["lon"],
    site_name      = as.character(nc$gatts$site_name %||% nc$gatts$title %||% NA),
    institution    = as.character(nc$gatts$institution %||% NA),
    processing_lvl = as.character(nc$gatts$processing_level %||% NA)
  )
  
  nc_close(nc)
  out
}


library(ncdf4)
library(purrr)
library(stringr)
library(dplyr)
`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

# Try to extract a FLUXNET-like site code from one file's globals
read_siteid_from_globals <- function(nc_path) {
  nc <- nc_open(nc_path)
  g <- nc$gatts
  nc_close(nc)
  
  # common attribute keys we might see
  keys <- c("site_id","siteid","site_code","fluxnet_id","fluxnet_site_id",
            "site","station_id","title","site_name")
  vals <- lapply(keys, function(k) g[[k]] %||% NA_character_)
  names(vals) <- keys
  x <- unlist(vals, use.names = TRUE)
  
  # Pick the first string that looks like AU-xxx, NZ-xxx, etc.
  m <- x[str_detect(x, "^[A-Z]{2}-[A-Za-z0-9]{3}$")]
  if (length(m)) return(unname(m[1]))
  
  # Sometimes embedded in a longer title, try to extract pattern AU-xxx
  y <- x[str_detect(x, "[A-Z]{2}-[A-Za-z0-9]{3}")]
  if (length(y)) {
    z <- stringr::str_extract(y[1], "[A-Z]{2}-[A-Za-z0-9]{3}")
    if (!is.na(z)) return(z)
  }
  
  NA_character_
}




meta_one <- read_nc_metadata(nc_path)
meta_one

meta_one %>% 
  select(file, lat, lon, start, end, n_time, time_units)
