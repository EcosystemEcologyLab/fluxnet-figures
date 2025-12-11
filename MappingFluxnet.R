# ================================
# Outline-Only Global Maps (Simple)
# ================================

# Packages
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(janitor)
library(scales)   # for hue_pal()

# (Optional) Avoid odd outline artifacts on some systems
old_s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old_s2), add = TRUE)

# ---- 1) Load & tidy site data ----
data_path <- "data/FLUXNET/EddyFluxsites_master_merged.csv"
ICOS_likely <- "data/ICOS_likelySitesDec9_2025.csv"

sites <- read_csv(data_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Normalize latitude/longitude column names if needed
lat_candidates <- c("latitude", "lat", "y", "decimal_latitude")
lon_candidates <- c("longitude", "lon", "long", "x", "decimal_longitude")
lat_col <- intersect(lat_candidates, names(sites))[1]
lon_col <- intersect(lon_candidates, names(sites))[1]
if (is.na(lat_col) || is.na(lon_col)) stop("Could not find latitude/longitude columns.")

sites <- sites |>
  rename(latitude = !!lat_col, longitude = !!lon_col) |>
  mutate(
    latitude  = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    network   = as.character(network)
  ) |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  # Fix 0–360 longitudes (if present)
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) |>
  filter(dplyr::between(latitude, -90, 90),
         dplyr::between(longitude, -180, 180))

stopifnot(nrow(sites) > 0)

# ---- 2) Basemap: outline-only land (no fill) ----
land <- rnaturalearth::ne_download(
  scale = "medium", type = "land", category = "physical", returnclass = "sf"
) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

# ---- 3) Shared aesthetics ----
blue_all   <- "#2C7FB8"  # all-sites blue (background)
col_orange <- "#E69F00"  # ICOS
col_crimson<- "#D0104C"  # AmeriFlux
col_green  <- "#7FC97F"  # OzFlux
cyan_f15   <- "#00FFFF"  # FLUXNET2015

pt_size   <- 2.4
pt_alpha  <- 0.70
pt_stroke <- 0.30

# Networks present in the sites table
present_networks <- sort(unique(sites$network))

# Start with an empty named vector
col_map <- setNames(rep(NA_character_, length(present_networks)), present_networks)

# Helper: assign colors to any network name that matches a pattern
assign_color_by_pattern <- function(map, pattern, col) {
  idx <- grepl(pattern, names(map), ignore.case = TRUE)
  map[idx] <- col
  map
}

# 1) Fix colors for key networks (pattern-based, robust to label variants)
col_map <- assign_color_by_pattern(col_map, "AmeriFlux",    col_crimson)
col_map <- assign_color_by_pattern(col_map, "ICOS",         col_orange)
col_map <- assign_color_by_pattern(col_map, "OzFlux",       col_green)
col_map <- assign_color_by_pattern(col_map, "FLUXNET2015",  cyan_f15)

# 2) Colors for the remaining networks (avoid cyan / blue-ish)
other_cols <- c(
  "#F0E442",  # yellow
  "#A6761D",  # brown
  "#999999",  # grey
  "#7570B3",  # purple
  "#E41A1C"   # red
)

remaining <- which(is.na(col_map))
if (length(remaining) > 0) {
  col_map[remaining] <- rep_len(other_cols, length(remaining))
}


# Subset for FLUXNET2015
sites_f15 <- dplyr::filter(sites, network == "FLUXNET2015")

# ---- 4) Base map builder (outline-only) ----
outline_base <- function(title = NULL) {
  ggplot() +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    ) +
    geom_sf(data = land, fill = NA, color = "black", linewidth = 0.30) +
    coord_sf(expand = FALSE) +
    labs(title = title)
}

# =========================
# (1) Global — All sites (blue)
# =========================
p1_all_blue <- outline_base("Global — All Sites (Blue)") +
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude),
    shape = 21, fill = blue_all, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  ) +
  theme(legend.position = "none")


# ---------------------------------------------
# 1) Filter AmeriFlux (FLUXNET registry) & OzFlux
# ---------------------------------------------
sites_amf_reg <- sites %>%
  dplyr::filter(network == "AmeriFlux (FLUXNET registry)")

sites_ozflux <- sites %>%
  dplyr::filter(network == "OzFlux")

# ---------------------------------------------
# 2) Build combined overlay map
#    (blue background + ICOS + AmeriFlux + OzFlux)
# ---------------------------------------------
p1_all_blue_overlay <- p1_all_blue +
  # ICOS likely sites (from earlier step, if you defined ICOS_likely_df)
  geom_point(
    data  = ICOS_likely_df,
    aes(x = longitude, y = latitude),
    shape  = 21,
    fill   = "#E69F00",   # orange
    color  = "black",
    size   = pt_size + 0.4,
    alpha  = 0.95,
    stroke = pt_stroke
  ) +
  # AmeriFlux (FLUXNET registry)
  geom_point(
    data  = sites_amf_reg,
    aes(x = longitude, y = latitude),
    shape  = 21,
    fill   = "#D0104C",   # magenta/pink
    color  = "black",
    size   = pt_size + 0.3,
    alpha  = 0.95,
    stroke = pt_stroke
  ) +
  # OzFlux
  geom_point(
    data  = sites_ozflux,
    aes(x = longitude, y = latitude),
    shape  = 21,
    fill   = "#7FC97F",   # green-ish
    color  = "black",
    size   = pt_size + 0.3,
    alpha  = 0.95,
    stroke = pt_stroke
  )

# View
p1_all_blue_overlay

# ---------------------------------------------
# (1b) Global — All blue + key 3 groups in cyan
#       AmeriFlux (FLUXNET registry), OzFlux, ICOS-likely
# ---------------------------------------------

# Sanity check: ICOS_likely_df must exist and have longitude/latitude
if (!exists("ICOS_likely_df")) {
  stop("ICOS_likely_df not found. Make sure you've read ICOS_likelySitesDec9_2025 into ICOS_likely_df with columns 'longitude' and 'latitude'.")
}

sites_special_cyan <- dplyr::bind_rows(
  # AmeriFlux (FLUXNET registry) + OzFlux from the main sites table
  sites %>%
    dplyr::filter(network %in% c(
      "AmeriFlux (FLUXNET registry)",
      "OzFlux"
    )) %>%
    dplyr::select(longitude, latitude),
  
  # ICOS likely sites from the separate file
  ICOS_likely_df %>%
    dplyr::select(longitude, latitude)
) %>%
  dplyr::distinct()

p1_all_blue_two_color <- outline_base("Likely FLUXNET Feb 2026") +
  # base: all sites in blue
  geom_point(
    data  = sites,
    aes(x = longitude, y = latitude),
    shape = 21,
    fill  = blue_all,
    color = "black",
    size  = pt_size,
    alpha = 1,
    stroke = pt_stroke
  ) +
  # overlay: only the three key groups in cyan
  geom_point(
    data  = sites_special_cyan,
    aes(x = longitude, y = latitude),
    shape = 21,
    fill  = cyan_f15,
    color = "black",
    size  = pt_size + 0.4,
    alpha = 0.95,
    stroke = pt_stroke
  ) +
  theme(legend.position = "none")

# Inspect
p1_all_blue_two_color

# ---------------------------------------------
# (1c) Global — All sites (blue) +
#      sites actually used in current analysis (cyan)
# ---------------------------------------------

# Collapse 'annual' to one row per site with coordinates
annual_sites <- annual %>%
  dplyr::filter(!is.na(LOCATION_LAT), !is.na(LOCATION_LONG), !is.na(IGBP)) %>%
  dplyr::distinct(
    site,
    latitude  = LOCATION_LAT,
    longitude = LOCATION_LONG
  ) %>%
  # keep longitudes consistent with main 'sites' table
  dplyr::mutate(
    longitude = ifelse(longitude > 180, longitude - 360, longitude)
  )

p1_all_blue_analysis <- outline_base("Global — Sites Used in Current Analysis") +
  # background: all potential sites in blue
  geom_point(
    data  = sites,
    aes(x = longitude, y = latitude),
    shape = 21,
    fill  = "grey",
    color = "black",
    size  = pt_size,
    alpha = pt_alpha,
    stroke = pt_stroke
  ) +
  # overlay: sites that actually appear in 'annual'
  geom_point(
    data  = annual_sites,
    aes(x = longitude, y = latitude),
    shape = 21,
    fill  = cyan_f15,     # same cyan as FLUXNET2015
    color = "black",
    size  = pt_size + 0.4,
    alpha = 0.95,
    stroke = pt_stroke
  ) +
  theme(legend.position = "none")

# Inspect
p1_all_blue_analysis
# =========================
# (2) Global — Colored by network
# =========================
p2_by_network <- outline_base("Global — By Network") +
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude, fill = network),
    shape = 21, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  ) +
  scale_fill_manual(values = col_map, name = "Network")

# =========================
# (3) Global — FLUXNET2015 only (cyan)
# =========================

cyan_f15 <- "#00FFFF"   # << your requested FLUXNET2015 color

p3_flux2015_only <- outline_base("FLUXNET 2015 Release") +
  geom_point(
    data = sites_f15,
    aes(x = longitude, y = latitude),
    shape = 21,
    fill  = cyan_f15,     # << updated here
    color = "black",
    size  = pt_size,
    alpha = pt_alpha,
    stroke = pt_stroke
  ) +
  theme(legend.position = "none")


# =========================
# (4) Global — All blue + FLUXNET2015 overlay (orange)
# =========================
p4_blue_plus_f15 <- outline_base("Global Potential Network") +
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude),
    shape = 21, fill = blue_all, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  ) +
  geom_point(
    data = sites_f15,
    aes(x = longitude, y = latitude),
    shape = 21, fill = cyan_f15, color = "black",
    size = pt_size + 0.1, alpha = min(pt_alpha + 0.1, 1), stroke = pt_stroke
  ) +
  theme(legend.position = "none")

# ---- 5) Print (or save) ----
p1_all_blue
p2_by_network
p3_flux2015_only
p4_blue_plus_f15

# Optional saves:
 ggsave("maps/map_1_all_blue_outline.png", p1_all_blue, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_1_2_likely_in_2025.png", p1_all_blue_overlay, width = 10, height = 6, dpi = 300) 
 ggsave("maps/map_1_4_two_color_key_groups.png",
        p1_all_blue_two_color, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_2_by_network_outline.png", p2_by_network, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_3_fluxnet2015_only_outline.png", p3_flux2015_only, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_4_all_blue_plus_fluxnet2015_outline.png", p4_blue_plus_f15, width = 10, height = 6, dpi = 300)

 
 # Your colors + labels
 col_orange  <- "#E69F00"  # ICOS
 col_crimson <- "#D0104C"  # Ameriflux
 col_green   <- "#7FC97F"  # OzFlux
 base_blue   <- "#2C7FB8"  # Additional published sites
 
 legend_colors <- c(
   "ICOS"                       = col_orange,
   "Ameriflux"                  = col_crimson,
   "OzFlux"                     = col_green,
   "'Potential' sites" = base_blue
 )
 
 # Dummy data: one point per legend entry
 legend_df <- tibble(
   x = 1,
   y = 1,
   group = factor(names(legend_colors), levels = names(legend_colors))
 )
 
 # Standalone legend plot
 p_legend_dots <- ggplot(legend_df, aes(x = x, y = y, color = group)) +
   geom_point(size = 6) +  # big dots
   scale_color_manual(values = legend_colors, name = NULL) +
   guides(color = guide_legend(override.aes = list(size = 6))) +
   theme_void(base_size = 14) +
   theme(
     legend.position = "right",
     legend.text = element_text(size = 14)
   )
 
 # Show it
 p_legend_dots
 
 # These come from your mapping code
 # col_map  <- named vector of network → hex colors
 # blue_all <- "#2C7FB8"
 
 # Create a display legend order
 legend_networks <- c(
   "ICOS",
   "AmeriFlux",
   "OzFlux",
   "FLUXNET2015",
   "AsiaFlux",
   "JapanFLUX",
   "WestAfrica",
   "Additional published sites"
 )
 
 # Build legend color vector using col_map + background blue
 legend_colors <- c(
   col_map[legend_networks[legend_networks %in% names(col_map)]],
   "Additional published sites" = blue_all
 )
 library(ggplot2)
library(tibble)

# Drop any missing names (not all networks appear in every dataset)
legend_colors <- legend_colors[!is.na(legend_colors)]

legend_df <- tibble(
  group = factor(names(legend_colors), levels = names(legend_colors)),
  x = 1,
  y = 1
)

p_legend_dots <- ggplot(legend_df, aes(x = x, y = y, fill = group)) +
  geom_point(
    shape  = 21,
    size   = 7,
    color  = "black",
    stroke = 0.7
  ) +
  scale_fill_manual(values = legend_colors, name = NULL) +
  guides(
    fill = guide_legend(
      override.aes = list(
        size  = 8,
        shape = 21,
        color = "black"
      )
    )
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 13),
    legend.key.size = unit(1.4, "lines")
  )

p_legend_dots

 
 
# =========================================
# Aridity-Background Maps (Simple & Clean)
# =========================================

# Packages
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)         # user prefers raster (not terra)
library(rnaturalearth)
library(janitor)

# (Optional) Avoid outline artifacts during draw
old_s2 <- sf::sf_use_s2(FALSE); on.exit(sf::sf_use_s2(old_s2), add = TRUE)

# ---------------------------
# 1) Load & tidy site data
# ---------------------------
data_path <- "data/FLUXNET/EddyFluxsites_master_merged.csv"

sites <- read_csv(data_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Normalize latitude/longitude column names if needed
lat_candidates <- c("latitude", "lat", "y", "decimal_latitude")
lon_candidates <- c("longitude", "lon", "long", "x", "decimal_longitude")
lat_col <- intersect(lat_candidates, names(sites))[1]
lon_col <- intersect(lon_candidates, names(sites))[1]
if (is.na(lat_col) || is.na(lon_col)) stop("Could not find latitude/longitude columns.")

sites <- sites |>
  rename(latitude = !!lat_col, longitude = !!lon_col) |>
  mutate(
    latitude  = suppressWarnings(as.numeric(latitude)),
    longitude = suppressWarnings(as.numeric(longitude)),
    network   = as.character(network)
  ) |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  # Fix 0–360 longitudes (if present)
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) |>
  filter(dplyr::between(latitude, -90, 90),
         dplyr::between(longitude, -180, 180))

# Subset for the NA map
sites_ameriflux_reg <- filter(sites, network == "AmeriFlux (FLUXNET registry)")

# ---------------------------
# 2) Basemap: outline-only land
# ---------------------------
land <- rnaturalearth::ne_download(
  scale = "medium", type = "land", category = "physical", returnclass = "sf"
) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

# ---------------------------
# 3) Aridity raster -> data frame(s)
# ---------------------------
# Path to your local raster (adjust if needed)
ai_path <- "~/Documents/RProjects/data/AI_4km.tif"

AI_4km <- raster(ai_path)

# Aggregate if plotting is slow (increase 'fact' for faster/lower-res)
AI_4km_agg <- aggregate(AI_4km, fact = 2)

# Helper to convert a raster to df and classify aridity classes
raster_to_aridity_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  # Rename value column to 'AI' (works even if source layer has a different name)
  names(df)[names(df) == names(r)] <- "AI"
  # Classify (same thresholds you used)
  df$Aridity_Class <- factor(
    ifelse(df$AI < 0.05, "Hyper-Arid",
           ifelse(df$AI < 0.20, "Arid",
                  ifelse(df$AI < 0.50, "Semi-Arid",
                         ifelse(df$AI < 0.65, "Dry Sub-Humid", "Humid")
                  )
           )
    ),
    levels = c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid","Humid")
  )
  df
}

# Global aridity df
ai_global_df <- raster_to_aridity_df(AI_4km_agg)

# North America extent (lon/lat)
na_xlim <- c(-170, -50)
na_ylim <- c(   5,  83)

# Crop raster to NA and convert
AI_4km_agg_na <- crop(AI_4km_agg, extent(na_xlim[1], na_xlim[2], na_ylim[1], na_ylim[2]))
ai_na_df <- raster_to_aridity_df(AI_4km_agg_na)

# ---------------------------
# 4) Shared aesthetics
# ---------------------------
blue_all <- "#2C7FB8"   # all-sites blue
orange_f15 <- "#E69F00" # (not used here, but kept for consistency)
pt_size  <- 2.2
pt_alpha <- 0.75
pt_stroke <- 0.30

# Aridity palette (simple & clear)
aridity_cols <- c(
  "Hyper-Arid"   = "red",
  "Arid"         = "orange",
  "Semi-Arid"    = "yellow",
  "Dry Sub-Humid"= "lightgreen",
  "Humid"        = "lightgrey"
)

# Reusable outline-only base (keeps oceans white)
outline_base <- function(title = NULL) {
  ggplot() +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    ) +
    coord_sf(expand = FALSE) +
    labs(title = title)
}

# ---------------------------
# 5) MAPS
# ---------------------------

# (A) GLOBAL: aridity background + ALL sites in blue
p_global_ai_all_blue <-
  outline_base("Global — Aridity Background with All Sites (Blue)") +
  # aridity background
  geom_raster(
    data = ai_global_df,
    aes(x = x, y = y, fill = Aridity_Class),
    alpha = 0.85
  ) +
  scale_fill_manual(values = aridity_cols, name = "Aridity Class") +
  # land outlines on top (no fill)
  geom_sf(data = land, fill = NA, color = "black", linewidth = 0.30) +
  # sites
  geom_point(
    data = sites,
    aes(x = longitude, y = latitude),
    shape = 21, fill = blue_all, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  )

# (B) NORTH AMERICA: aridity background + AmeriFlux (FLUXNET registry) only
p_na_ai_ameriflux <-
  outline_base("North America — Aridity Background with AmeriFlux (FLUXNET registry)") +
  # aridity background (NA crop)
  geom_raster(
    data = ai_na_df,
    aes(x = x, y = y, fill = Aridity_Class),
    alpha = 0.85
  ) +
  scale_fill_manual(values = aridity_cols, name = "Aridity Class") +
  # land outlines on top
  geom_sf(data = land, fill = NA, color = "black", linewidth = 0.30) +
  # sites (NA filter via coord limits)
  geom_point(
    data = sites_ameriflux_reg,
    aes(x = longitude, y = latitude),
    shape = 21, fill = blue_all, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  ) +
  coord_sf(xlim = na_xlim, ylim = na_ylim, expand = FALSE)




#---------------------
# ARID ONLY
#---------------------

# --- Classify sites by AI and filter out "Humid" -----------------------------

# Reuse the same thresholds as raster_to_aridity_df()
classify_ai_vec <- function(ai_vec) {
  factor(
    dplyr::case_when(
      is.na(ai_vec)        ~ NA_character_,
      ai_vec < 0.05        ~ "Hyper-Arid",
      ai_vec < 0.20        ~ "Arid",
      ai_vec < 0.50        ~ "Semi-Arid",
      ai_vec < 0.65        ~ "Dry Sub-Humid",
      TRUE                 ~ "Humid"
    ),
    levels = c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid","Humid")
  )
}

# Extract AI at each site (use the aggregated raster for speed, or AI_4km for full res)
sites <- sites |>
  mutate(
    AI_at_site = raster::extract(AI_4km_agg, cbind(longitude, latitude)),
    Aridity_Class_site = classify_ai_vec(AI_at_site)
  )

# Keep only sites not in Humid
sites_non_humid <- sites |>
  filter(!is.na(Aridity_Class_site), Aridity_Class_site != "Humid")

# --- Plot: Global aridity background + NON-HUMID sites only ------------------

p_global_ai_non_humid_only <-
  outline_base("Global — Aridity Background with Non-Humid Sites Only") +
  geom_raster(
    data = ai_global_df,
    aes(x = x, y = y, fill = Aridity_Class),
    alpha = 0.85
  ) +
  scale_fill_manual(values = aridity_cols, name = "Aridity Class") +
  geom_sf(data = land, fill = NA, color = "black", linewidth = 0.30) +
  geom_point(
    data = sites_non_humid,
    aes(x = longitude, y = latitude),
    shape = 21, fill = blue_all, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
  )

# Show or save
p_global_ai_non_humid_only
# ggsave("maps/global_aridity_non_humid_sites.png", p_global_ai_non_humid_only, width = 11, height = 6.5, dpi = 300)


# ---------------------------
# 6) Show (or save)
# ---------------------------
p_global_ai_all_blue
p_na_ai_ameriflux

# Optional saves:
 ggsave("maps/global_aridity_all_blue.png", p_global_ai_all_blue, width = 11, height = 6.5, dpi = 300)
 ggsave("maps/north_america_aridity_ameriflux_registry.png", p_na_ai_ameriflux, width = 10, height = 7, dpi = 300)

 
 
 
 
 library(sf)
 library(dplyr)
 library(readr)
 library(ggplot2)
 library(stringr)
 library(scales)
 
 # 1) Download & read county boundaries
 zip_url <- "https://www2.census.gov/geo/tiger/GENZ2024/shp/cb_2024_us_county_5m.zip"
 zip_file <- tempfile(fileext = ".zip")
 download.file(zip_url, zip_file, mode = "wb")
 unzip(zip_file, exdir = tempdir())
 counties <- st_read(list.files(tempdir(), pattern = "cb_2024_us_county_5m\\.shp$", full.names = TRUE), quiet = TRUE)
 
 # 2) Read county population data (Vintage 2024)
 pop_url <- "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/CO-EST2024-ALLDATA.csv"
 pop <- read_csv(pop_url, show_col_types = FALSE) |>
   mutate(
     STATEFP = str_pad(STATE, 2, pad = "0"),
     COUNTYFP = str_pad(COUNTY, 3, pad = "0"),
     GEOID = paste0(STATEFP, COUNTYFP)
   ) |>
   select(GEOID, POPESTIMATE2024)
 
 # 3) Join to shapefile
 mapdat <- counties |>
   mutate(GEOID = as.character(GEOID)) |>
   left_join(pop, by = "GEOID")
 
 # 4) Plot
 ggplot(mapdat) +
   geom_sf(aes(fill = POPESTIMATE2024), linewidth = 0) +
   scale_fill_viridis_c(trans = "log10", labels = label_comma(), option = "C") +
   labs(title = "U.S. County Population (Vintage 2024)",
        subtitle = "Population estimates as of July 1, 2024",
        fill = "Population") +
   theme_minimal() +
   theme(axis.text = element_blank(),
         panel.grid = element_blank())
 