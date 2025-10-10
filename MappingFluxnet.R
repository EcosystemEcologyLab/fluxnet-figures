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
blue_all <- "#2C7FB8"  # all-sites blue
cyan_f15 <- "#E69F00"  # FLUXNET2015 cyan
pt_size  <- 2.4
pt_alpha <- 0.70
pt_stroke <- 0.30

# Palette for networks (auto if unknowns)
present_networks <- sort(unique(sites$network))
col_map <- setNames(hue_pal()(length(present_networks)), present_networks)
# If you prefer a specific color for FLUXNET2015 in map (2), uncomment:
# col_map["FLUXNET2015"] <- "#9467bd"

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
# (3) Global — FLUXNET2015 only (orange)
# =========================
p3_flux2015_only <- outline_base("FLUXNET 2015 Release") +
  geom_point(
    data = sites_f15,
    aes(x = longitude, y = latitude),
    shape = 21, fill = cyan_f15, color = "black",
    size = pt_size, alpha = pt_alpha, stroke = pt_stroke
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
 ggsave("maps/map_2_by_network_outline.png", p2_by_network, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_3_fluxnet2015_only_outline.png", p3_flux2015_only, width = 10, height = 6, dpi = 300)
 ggsave("maps/map_4_all_blue_plus_fluxnet2015_outline.png", p4_blue_plus_f15, width = 10, height = 6, dpi = 300)

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

