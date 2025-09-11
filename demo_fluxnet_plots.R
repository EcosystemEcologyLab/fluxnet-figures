# demo_fluxnet_plots.R
# ------------------------
# Demonstration script for FLUXNET plotting functions
# Requires: fcn_utility_FLUXNET.R and fcn_plot_FLUXNET.R
# ------------------------

# ---- Load source scripts (assumes they are in working directory) ----
# Project utilities and plotting helpers
source(file = "R/fcn_utility_FLUXNET.R")
source(file = "R/fcn_plot_FLUXNET.R")

# -----------------------------------------------------------------------------
# 1) Discover and load site metadata
#    - Pulls AmeriFlux + ICOS site info; harmonizes fields (IGBP, LAT/LON, etc.)
# -----------------------------------------------------------------------------
metadata <- load_fluxnet_metadata()
## Warning message about "UK" stems from countrycode ambiguity — OK to note.

# -----------------------------------------------------------------------------
# 2) Discover locally available AMF/ICOS files and build a file manifest
#    - Manifest encodes site, dataset type (FULLSET, L2), time integral (YY, etc.)
# -----------------------------------------------------------------------------
amf_files  <- discover_AMF_files(data_dir = here::here("data/FLUXNET/AMF"))
icos_files <- discover_ICOS_files(data_dir = here::here("data/FLUXNET/ICOS"))

# Combine and de-duplicate rows that differ only by internal path bookkeeping
manifest <- bind_rows(amf_files, icos_files) %>%
  distinct(
    site, data_product, dataset, time_integral, start_year, end_year,
    .keep_all = TRUE
  )

# -----------------------------------------------------------------------------
# 3) Load annual (YY) FULLSET data using the manifest
#    - Replaces -9999 sentinels with NA
#    - Adds integer year column
#    - Joins site metadata (IGBP, LAT/LON, etc.) to the annual records
# -----------------------------------------------------------------------------
annual <- manifest %>%
  filter(time_integral == "YY", dataset == "FULLSET") %>%
  load_fluxnet_data() %>%                                    # reads 383 files here
  mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%# sentinel → NA
  mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
  left_join(metadata %>% select(-SITEID, -SITE_ID), by = join_by(site))




# ---- Plot 1a: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(plots_igbp$composite_plot)

# ---- Plot 1b: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "GPP_NT_VUT_REF")
print(plots_igbp$composite_plot)

# ---- Plot 1c: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
cat("Generating flux summary by IGBP...\n")
plots_igbp <- plot_flux_by_igbp(annual, flux_var = "RECO_NT_VUT_REF")
print(plots_igbp$composite_plot)

# # ---- Plot 1d: IGBP Aggregated Flux Summary (Boxplot, Median, Count) ----
# cat("Generating flux summary by IGBP...\n")
# plots_igbp <- plot_flux_by_igbp(annual, flux_var = "WUE")
# print(plots_igbp$composite_plot)


# ---- Plot 2: Flux by IGBP and Time Slice (5-year bins) ----
cat("Generating time-sliced flux comparison...\n")
plots_timeslice <- plot_flux_by_igbp_timeslice_grouped(annual, flux_var = "NEE_VUT_REF")
print(plots_timeslice$flux_plot)

# ---- Plot 3: Interannual Boxplots Grouped by IGBP Type ----
cat("Generating group-wise annual flux boxplots...\n")
boxplots <- plot_flux_box_by_group(annual, flux_var = "RECO_NT_VUT_REF", y_mode = "squish")
print(boxplots$Forest)
print(boxplots$ShrubOpens)
print(boxplots$GrassCropsWet)



# ---- Plot 4a: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median NEE by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(ts_plot)

# ---- Plot 4b: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median GPP by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "GPP_NT_VUT_REF")
print(ts_plot)

# ---- Plot 4c: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median RECO by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "RECO_NT_VUT_REF")
print(ts_plot)

# ---- Plot 4d: Timeseries of Median Fluxes by IGBP ----
cat("Generating timeseries of median WUE by IGBP...\n")
ts_plot <- plot_flux_timeseries_by_igbp(annual, flux_var = "WUE")
print(ts_plot)

# ---- Plot 5: Latitudinal Summary (Ribbon + Points) ----
#5a 
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "NEE_VUT_REF")
print(lat_plot)

#5b
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "GPP_NT_VUT_REF")
print(lat_plot)

#5c
cat("Generating latitudinal flux summary...\n")
lat_plot <- plot_latitudinal_flux(annual, metadata, flux_var = "RECO_NT_VUT_REF")
print(lat_plot)


# # ---- Plot 6: Seasonal Climatology by IGBP Group ----
# cat("Generating daily seasonal cycle plots...\n")
# seasonal_cycle <- plot_seasonal_cycle(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")
# print(seasonal_cycle$Forest)

# ---- Plot 7: Scatterplots of Climate vs Flux ----
cat("Generating annual precipitation vs NEE and temperature vs GPP...\n")
climate_plots <- plot_annual_fluxnet_data(annual)
print(climate_plots$precip_vs_nee)
print(climate_plots$temp_vs_gpp)


# ---- Plot 8: General XY Scatter (GPP vs LE) ----
cat("Generating general scatterplot of GPP vs LE...\n")
scatter <- PlotXY_annual(annual, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS")
print(scatter)

scatter <- PlotXY_annual(annual, x_var = "NEE_VUT_REF", y_var = "LE_F_MDS")
print(scatter)

# ---- Optional: Save plots to folder ----
# Uncomment to save any plot list, e.g.:
# save_plot_list(plots_igbp, prefix = "flux_by_IGBP")
# save_plot_list(boxplots, prefix = "grouped_boxplot")

cat("All plots generated successfully.\n")



# -----------------------------------------------------------------------------
# 4) Compute site-level climate & flux summaries from tower data
#    - Mean P_F (mm/yr), TA_F (°C), GPP, NEE per site
#    - Derive precipitation in cm/yr for Whittaker axes
#    - Tag sink/source by sign of mean NEE
#    - Re-attach site metadata to the site summaries
# -----------------------------------------------------------------------------
climate_summary <- annual %>%
  group_by(site) %>%
  summarize(
    mean_precip = mean(P_F, na.rm = TRUE),       # mm yr-1 (tower)
    mean_temp   = mean(TA_F, na.rm = TRUE),      # °C (tower)
    mean_GPP    = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    mean_NEE    = mean(NEE_VUT_REF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_precip_cm = set_units(mean_precip, "mm") |> set_units("cm"), # mm → cm
    NEE_sign = ifelse(mean_NEE < 0, "Sink (NEE < 0)", "Source (NEE ≥ 0)")
  ) %>%
  left_join(metadata, by = c("site" = "SITE_ID"))

# -----------------------------------------------------------------------------
# 5) Extract WorldClim climate at tower locations
#    - Uses WorldClim v2.1 bioclim: bio1 (MAT*10), bio12 (MAP mm)
#    - Converts MAP mm → cm for Whittaker y-axis
# -----------------------------------------------------------------------------
wc <- readRDS("data/wc_worldclim_30s.rds")  # pre-downloaded SpatRaster

# Build point geometry from site coordinates (EPSG:4326)
site_pts <- terra::vect(
  climate_summary,
  geom = c("LOCATION_LONG", "LOCATION_LAT"),
  crs  = "EPSG:4326"
)

# Extract bio1 and bio12 at each site (MAT*10, MAP mm)
wc_extract <- terra::extract(wc[[c(1, 12)]], site_pts)

# Attach WorldClim fields (note: MAT_WorldClim is bio1; scale as needed)
climate_summary$MAT_WorldClim      <- wc_extract[["wc2.1_30s_bio_1"]]   # MAT * 10 (°C*10)
climate_summary$MAP_WorldClim      <- wc_extract[["wc2.1_30s_bio_12"]]  # MAP (mm)
climate_summary$MAP_WorldClim_cm   <- climate_summary$MAP_WorldClim / 10# mm → cm

# -----------------------------------------------------------------------------
# 6) Simple tower-vs-WorldClim climate checks
#    - Scatter against 1:1 line for MAT and MAP
# -----------------------------------------------------------------------------
ggplot(climate_summary, aes(x = MAT_WorldClim, y = mean_temp)) +
  geom_point(color = "#1f78b4", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "WorldClim MAT (°C*10 or °C depending on scale)",  # adjust label if you rescale
       y = "Observed Site MAT (°C)") +
  theme_classic()

ggplot(climate_summary, aes(x = MAP_WorldClim, y = mean_precip)) +
  geom_point(color = "#33a02c", size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "WorldClim MAP (mm)", y = "Observed Site MAP (mm)") +
  theme_classic()

# -----------------------------------------------------------------------------
# 7) Whittaker biome overlay + site points
#    - Base polygons from plotbiomes::Whittaker_biomes (temp °C, precip cm)
#    - Overlay tower sites colored by IGBP
# -----------------------------------------------------------------------------
library(plotbiomes)
library(ggnewscale)

data("Whittaker_biomes")

my_15_colors <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
  "#e6ab02", "#a6761d", "#666666", "#1f78b4", "#b2df8a",
  "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99", "#8dd3c7"
)

ggplot() +
  geom_polygon(
    data = Whittaker_biomes,
    aes(x = temp_c, y = precp_cm, group = biome, fill = biome),
    color = "grey80", alpha = 0.4
  ) +
  scale_fill_brewer(palette = "BrBG", name = "Biome") +
  new_scale_fill() +
  geom_point(
    data = climate_summary,
    aes(x = MAT_WorldClim, y = MAP_WorldClim_cm, color = IGBP),
    size = 3, alpha = 0.9
  ) +
  scale_color_manual(values = my_15_colors) +
  labs(x = "Temperature (°C)", y = "Precipitation (cm yr⁻¹)") +
  theme_classic()

# -----------------------------------------------------------------------------
# 8) Whittaker with NEE emphasis (bubble size = |NEE|, fill = sink/source)
# -----------------------------------------------------------------------------
ggplot() +
  geom_polygon(
    data = Whittaker_biomes,
    aes(x = temp_c, y = precp_cm, group = biome, fill = biome),
    color = "grey80", alpha = 0.4
  ) +
  scale_fill_brewer(palette = "BrBG", name = "Biome") +
  new_scale_fill() +
  geom_point(
    data = climate_summary,
    aes(x = MAT_WorldClim, y = MAP_WorldClim_cm,
        size = abs(mean_NEE), fill = NEE_sign),
    shape = 21, color = "black", alpha = 0.9
  ) +
  scale_fill_manual(
    values = c("Sink (NEE < 0)" = "#1b9e77",
               "Source (NEE ≥ 0)" = "#d95f02"),
    name = "NEE Sign"
  ) +
  scale_size_continuous(name = "|NEE|", range = c(1, 8)) +
  labs(x = "Temperature (°C)", y = "Precipitation (cm yr⁻¹)") +
  theme_classic()

