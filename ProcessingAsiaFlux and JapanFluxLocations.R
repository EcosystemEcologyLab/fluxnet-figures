# ---- Packages (explicit) ----
# install.packages(c("sf","dplyr","readr","countrycode","rnaturalearth","rnaturalearthdata"))
invisible(lapply(
  c("sf","dplyr","readr","countrycode","rnaturalearth","rnaturalearthdata"),
  require, character.only = TRUE
))

# ---- Config ----
input_csv  <- "data/FLUXNET/AsiaFlux_JapanFlux_2025.csv"
output_csv <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_with_country_matchcheck.csv"

# Prefer strict point-in-polygon (st_within). If border-touching points should count, set to "intersects".
join_mode <- "within"  # "within" or "intersects"

# ---- Helpers ----
to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9\\-\\.]+", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

infer_iso2_from_id <- function(site_id) {
  ifelse(
    is.na(site_id),
    NA_character_,
    {
      m <- regmatches(site_id, regexpr("^[A-Z]{2}(?=-)", site_id, perl = TRUE))
      out <- ifelse(length(m) == 0 | m == "", NA_character_, m)
      out
    }
  )
}

normalize_iso2 <- function(code) {
  code <- toupper(code)
  dplyr::case_when(
    code == "UK" ~ "GB",  # legacy/ambiguous
    TRUE ~ code
  )
}

# Return the suffix of Site_ID after the first "CC-" prefix if present.
# e.g., "JP-Api" -> "Api"; "CN-HFK" -> "HFK"; "OM-BDR" -> "BDR"
site_suffix <- function(site_id) {
  ifelse(
    is.na(site_id),
    NA_character_,
    sub("^[A-Z]{2}-", "", site_id)
  )
}

# ---- 1) Read sites ----
sites <- readr::read_csv(input_csv, show_col_types = FALSE)

# ---- 2) Coerce fields safely ----
sites <- dplyr::mutate(
  .data = sites,
  Latitude    = to_numeric(.data$Latitude),
  Longitude   = to_numeric(.data$Longitude),
  Elevation_m = to_numeric(.data$Elevation_m)
)

# ---- 3) Load country boundaries (admin-0) and prep geometry ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- sf::st_make_valid(world)
world <- sf::st_transform(world, 4326)
world <- dplyr::filter(world, .data$continent != "Antarctica")
world <- dplyr::select(world, name_long, iso_a2, iso_a3, continent, geometry)

# ---- 4) Convert sites with valid coordinates to sf points ----
has_coords <- !is.na(sites$Latitude) & !is.na(sites$Longitude)
sites_pts  <- sites[has_coords, , drop = FALSE]
sites_pts  <- sf::st_as_sf(sites_pts, coords = c("Longitude", "Latitude"),
                           crs = 4326, remove = FALSE)

# ---- 5) Spatial join to assign country/continent ----
if (nrow(sites_pts) > 0) {
  sites_joined <- if (join_mode == "intersects") {
    sf::st_join(sites_pts, world, join = sf::st_intersects, left = TRUE)
  } else {
    sf::st_join(sites_pts, world, join = sf::st_within, left = TRUE)
  }
} else {
  sites_joined <- sites_pts
}

# ---- 6) Merge results back into the full data frame ----
sites_aug <- sites
sites_aug$Country_from_coords    <- NA_character_
sites_aug$ISO_A2_from_coords     <- NA_character_
sites_aug$ISO_A3_from_coords     <- NA_character_
sites_aug$Continent_from_coords  <- NA_character_

if (nrow(sites_pts) > 0) {
  idx <- which(has_coords)
  sites_aug$Country_from_coords[idx]   <- sites_joined$name_long
  sites_aug$ISO_A2_from_coords[idx]    <- sites_joined$iso_a2
  sites_aug$ISO_A3_from_coords[idx]    <- sites_joined$iso_a3
  sites_aug$Continent_from_coords[idx] <- sites_joined$continent
}

# ---- 7) Fallback from Site_ID prefix (e.g., "JP-***" -> "JP") ----
sites_aug <- dplyr::mutate(
  .data = sites_aug,
  ISO_A2_fallback = dplyr::if_else(
    is.na(.data$ISO_A2_from_coords),
    normalize_iso2(infer_iso2_from_id(.data$Site_ID)),
    NA_character_
  ),
  Country_fallback = countrycode::countrycode(.data$ISO_A2_fallback,
                                              origin = "iso2c",
                                              destination = "country.name"),
  ISO_A3_fallback  = countrycode::countrycode(.data$ISO_A2_fallback,
                                              origin = "iso2c",
                                              destination = "iso3c"),
  Continent_fallback = countrycode::countrycode(.data$ISO_A2_fallback,
                                                origin = "iso2c",
                                                destination = "continent")
)

# ---- 8) Final consolidated columns (prefer coords > fallback) ----
sites_final <- dplyr::mutate(
  .data = sites_aug,
  Country   = dplyr::coalesce(.data$Country_from_coords,   .data$Country_fallback),
  ISO_A2    = dplyr::coalesce(.data$ISO_A2_from_coords,    .data$ISO_A2_fallback),
  ISO_A3    = dplyr::coalesce(.data$ISO_A3_from_coords,    .data$ISO_A3_fallback),
  Continent = dplyr::coalesce(.data$Continent_from_coords, .data$Continent_fallback),
  Country_assignment = ifelse(is.na(.data$Country), "UNASSIGNED", "OK")
)

# ---- 9) Compare Site_ID prefix to ISO_A2 ----
sites_final <- dplyr::mutate(
  .data = sites_final,
  SiteID_prefix = substr(.data$Site_ID, 1, 2),
  ISO_match     = dplyr::case_when(
    is.na(.data$ISO_A2) ~ FALSE,                               # no ISO_A2 to compare
    is.na(.data$SiteID_prefix) ~ FALSE,                        # no prefix
    .data$SiteID_prefix == .data$ISO_A2 ~ TRUE,
    TRUE ~ FALSE
  )
)

# ---- 10) Construct the unified Site column per your rule ----
# If ISO_match == TRUE → Site = Site_ID
# If ISO_match == FALSE → Site = paste0(ISO_A2, "-", suffix_of_Site_ID)
sites_final <- dplyr::mutate(
  .data = sites_final,
  Site_suffix = site_suffix(.data$Site_ID),
  Site = dplyr::if_else(
    .data$ISO_match,
    .data$Site_ID,
    dplyr::if_else(
      !is.na(.data$ISO_A2) & !is.na(.data$Site_suffix) & .data$Site_suffix != "",
      paste0(.data$ISO_A2, "-", .data$Site_suffix),
      # If we can't form a clean suffix, fall back to ISO_A2-Site_ID (still explicit)
      dplyr::if_else(!is.na(.data$ISO_A2) & !is.na(.data$Site_ID),
                     paste0(.data$ISO_A2, "-", .data$Site_ID),
                     .data$Site_ID)
    )
  )
)

# ---- 11) Arrange and write out ----
keep_cols <- intersect(
  c("Site","Site_Name","Site_ID","Location","Climate","IGBP","Latitude","Longitude","Elevation_m","source",
    "Country","ISO_A2","ISO_A3","Continent","Country_assignment","SiteID_prefix","ISO_match"),
  names(sites_final)
)

sites_out <- dplyr::select(sites_final, dplyr::all_of(keep_cols))

readr::write_csv(sites_out, output_csv)
message("Wrote: ", normalizePath(output_csv))

# ---- 12) Optional quick checks ----
# Mismatches to inspect
mismatch_summary <- sites_out %>%
  dplyr::filter(.data$ISO_match == FALSE) %>%
  dplyr::select(.data$Site, .data$Site_Name, .data$Site_ID, .data$ISO_A2, .data$Country, .data$Continent)

print(utils::head(mismatch_summary, 20L))

# Coverage by country
summary_by_country <- dplyr::count(sites_out, .data$Country, sort = TRUE)
print(utils::head(summary_by_country, 20L))

#####
# ---- Packages (explicit) ----
# install.packages(c("dplyr","readr","stringr"))
invisible(lapply(c("dplyr","readr","stringr"), require, character.only = TRUE))

# ---- Config ----
input_csv          <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_with_country_matchcheck.csv"
output_dedup_csv   <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_dedup_by_location.csv"
output_report_csv  <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_duplicate_locations_report.csv"

# Rounding precision (decimal places) for lat/lon to define "identical" location
# Increase to 6–7 for stricter, reduce to 3–4 for looser matching
coord_decimals <- 5

# ---- Helpers ----
to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9\\-\\.]+", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

# ---- 1) Read data ----
sites <- readr::read_csv(input_csv, show_col_types = FALSE)

# ---- 2) Coerce lat/lon numeric and create location keys ----
sites <- dplyr::mutate(
  .data = sites,
  Latitude  = to_numeric(.data$Latitude),
  Longitude = to_numeric(.data$Longitude)
)

# Keep rows with coords and those without coords separate (we only dedupe by location for those with coords)
sites_with_coords <- dplyr::filter(sites, !is.na(.data$Latitude) & !is.na(.data$Longitude))
sites_no_coords   <- dplyr::filter(sites,  is.na(.data$Latitude) |  is.na(.data$Longitude))

sites_with_coords <- dplyr::mutate(
  .data = sites_with_coords,
  lat_key = round(.data$Latitude,  coord_decimals),
  lon_key = round(.data$Longitude, coord_decimals)
)

# ---- 3) Identify duplicate locations and choose which row to keep ----
# Preference: JapanFLUX first; tie-break by having Site_ID (non-NA), then alphabetical Site_ID, then Site_Name
sites_ranked <- sites_with_coords %>%
  dplyr::mutate(
    source_rank = dplyr::case_when(
      .data$source == "JapanFLUX" ~ 1L,
      TRUE                       ~ 2L
    ),
    has_siteid = !is.na(.data$Site_ID)
  ) %>%
  dplyr::group_by(.data$lat_key, .data$lon_key) %>%
  dplyr::arrange(.by_group = TRUE,
                 .data$source_rank,
                 dplyr::desc(.data$has_siteid),   # keep rows that have a Site_ID
                 .data$Site_ID,                   # then alphabetical Site_ID
                 .data$Site_Name) %>%             # then alphabetical Site_Name
  dplyr::mutate(
    keep_flag = dplyr::row_number() == 1L
  ) %>%
  dplyr::ungroup()

# ---- 4) Build a duplicates report (all rows at duplicate locations, with which one kept) ----
dupe_groups <- sites_ranked %>%
  dplyr::group_by(.data$lat_key, .data$lon_key) %>%
  dplyr::filter(dplyr::n() > 1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    .data$lat_key, .data$lon_key,
    .data$Site_Name, .data$Site_ID, .data$source,
    .data$Latitude, .data$Longitude,
    .data$Country, .data$ISO_A2, .data$ISO_A3,
    .data$Continent,
    .data$keep_flag
  )

# ---- 5) Keep the preferred row per identical location and recombine with no-coord rows ----
sites_with_coords_kept <- dplyr::filter(sites_ranked, .data$keep_flag) %>%
  dplyr::select(-.data$source_rank, -.data$has_siteid, -.data$keep_flag, -.data$lat_key, -.data$lon_key)

sites_dedup <- dplyr::bind_rows(sites_with_coords_kept, sites_no_coords)

# Optional: sort for readability
sites_dedup <- dplyr::arrange(sites_dedup, .data$source, .data$Country, .data$Site_Name, .data$Site_ID)

# ---- 6) Save outputs ----
readr::write_csv(sites_dedup,  output_dedup_csv)
readr::write_csv(dupe_groups,  output_report_csv)

# ---- 7) Console summary ----
total_n        <- nrow(sites)
with_coords_n  <- nrow(sites_with_coords)
no_coords_n    <- nrow(sites_no_coords)
kept_n         <- nrow(sites_with_coords_kept)
removed_n      <- with_coords_n - kept_n

message("Total rows: ", total_n)
message("Rows with coords: ", with_coords_n, " | without coords: ", no_coords_n)
message("Duplicate-location groups: ", dplyr::n_distinct(dplyr::select(dupe_groups, .data$lat_key, .data$lon_key)))
message("Removed (due to identical locations): ", removed_n)
message("Wrote deduped CSV: ", normalizePath(output_dedup_csv))
message("Wrote duplicates report: ", normalizePath(output_report_csv))







########PLOTS

# ---- Packages (explicit) ----
# install.packages(c("sf","ggplot2","readr","rnaturalearth","rnaturalearthdata"))
invisible(lapply(
  c("sf","ggplot2","readr","rnaturalearth","rnaturalearthdata"),
  require, character.only = TRUE
))

# ---- Input ----
input_csv <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_with_country_matchcheck.csv"
input_csv <- "data/FLUXNET/AsiaFlux_JapanFlux_2025_dedup_by_location.csv"


# ---- Read + prep points ----
sites_df <- readr::read_csv(input_csv, show_col_types = FALSE)

# Ensure numeric and drop rows without coordinates
to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[^0-9\\-\\.]+", "", as.character(x))
  suppressWarnings(as.numeric(x))
}

sites_df$Latitude  <- to_numeric(sites_df$Latitude)
sites_df$Longitude <- to_numeric(sites_df$Longitude)

sites_pts <- sites_df[!is.na(sites_df$Latitude) & !is.na(sites_df$Longitude), , drop = FALSE]
sites_sf  <- sf::st_as_sf(sites_pts, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)

# ---- Base world layer ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- sf::st_make_valid(world)
world <- sf::st_transform(world, 4326)

# ---- Aesthetics ----
# Two distinct colors; semi-transparent points for overlap visibility
source_colors <- c("AsiaFlux" = "#1f77b4", "JapanFLUX" = "#d62728")  # blue / red
pt_alpha <- 0.55
pt_size  <- 2.6
pt_shape <- 16  # solid circle

# ---- Global map ----
p_global <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "grey95", color = "grey80", linewidth = 0.2) +
  ggplot2::geom_point(
    data = sites_sf,
    mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude, color = .data$source),
    alpha = pt_alpha, size = pt_size, shape = pt_shape
  ) +
  ggplot2::scale_color_manual(values = source_colors, name = "Source") +
  ggplot2::coord_sf(expand = FALSE) +
  ggplot2::labs(title = "AsiaFlux + JapanFLUX Sites (Global)") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# ---- Asia-focused map ----
# Tweak the window if you want a different framing (e.g., include Middle East / SE Asia more)
asia_xlim <- c(20, 150)   # longitude
asia_ylim <- c(-10, 60)   # latitude

p_asia <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "grey95", color = "grey80", linewidth = 0.2) +
  ggplot2::geom_point(
    data = sites_sf,
    mapping = ggplot2::aes(x = .data$Longitude, y = .data$Latitude, color = .data$source),
    alpha = pt_alpha, size = pt_size, shape = pt_shape
  ) +
  ggplot2::scale_color_manual(values = source_colors, name = "Source") +
  ggplot2::coord_sf(
    xlim = asia_xlim,
    ylim = asia_ylim,
    expand = FALSE
  ) +
  ggplot2::labs(title = "AsiaFlux + JapanFLUX Sites (Asia Focus)") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    legend.position = "bottom"
  )

# ---- Print to viewer ----
print(p_global)
print(p_asia)
