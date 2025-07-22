# fluxnet_lai_crossref.R
# Workflow to cross-reference FLUXNET annual data with CV_LAI categorical raster

# Load libraries
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(terra)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)


# Source utility and plotting scripts
source("R/fcn_utility_FLUXNET.R")
source("R/fcn_plot_FLUXNET.R")

# Load metadata
site_metadata <- load_fluxnet_metadata()

# Load and clean data
annual_data <- load_and_clean_annual_data(site_metadata)
#daily_data  <- load_and_clean_daily_data(site_metadata)


# Load FLUXNET annual data and metadata
# annual_data <- readRDS("data/multiple_sites_annual.rds")
# load_and_clean_annual_data()


# Load the CV_LAI categorical raster
CV_LAI <- rast("~/Documents/RProjects/data/Wen_CVRAINFALL/CV_rainfall_beta_LAI_composite.tif")

# Convert site coordinates to sf and extract categories
site_coords <- site_metadata %>%
  filter(!is.na(LOCATION_LAT), !is.na(LOCATION_LONG)) %>%
  st_as_sf(coords = c("LOCATION_LONG", "LOCATION_LAT"), crs = 4326)

site_coords$CV_LAI_cat <- terra::extract(CV_LAI, vect(site_coords))[,2]

# Map category numbers to labels
cv_labels <- c(
  "CVLAI+CVIAVppt+LAIppt_m+",
  "CVLAI+CVIAVppt+LAIppt_m-",
  "CVLAI+CVIAVppt-LAIppt_m+",
  "CVLAI+CVIAVppt-LAIppt_m-",
  "CVLAI-CVIAVppt+LAIppt_m+",
  "CVLAI-CVIAVppt+LAIppt_m-",
  "CVLAI-CVIAVppt-LAIppt_m+",
  "CVLAI-CVIAVppt-LAIppt_m-"
)

# Convert raster to data frame for plotting
CV_LAI_df <- as.data.frame(CV_LAI, xy = TRUE, na.rm = TRUE)
names(CV_LAI_df)[3] <- "category"

CV_LAI_df$label <- factor(cv_labels[CV_LAI_df$category], levels = cv_labels)

site_coords$CV_LAI_label <- factor(cv_labels[site_coords$CV_LAI_cat], levels = cv_labels)



# Extract category and attach label
site_coords$CV_LAI_cat <- terra::extract(CV_LAI, vect(site_coords))[,2]
site_coords$CV_LAI_label <- factor(cv_labels[site_coords$CV_LAI_cat], levels = cv_labels)

# Get continent outlines
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot
ggplot() +
  geom_raster(data = CV_LAI_df, aes(x = x, y = y, fill = label)) +
  geom_sf(data = world, fill = NA, color = "gray30", size = 0.3) +
  geom_sf(data = site_coords, aes(color = CV_LAI_label), size = 1.8, shape = 21, fill = "white") +
  scale_fill_viridis_d(name = "CV_LAI Condition") +
  scale_color_manual(values = rep("black", 8), guide = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 13) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = "Site Locations and CV_LAI Conditions")



# ---- Add Parsing to Create Grouping Variables ----
site_coords <- site_coords %>%
  mutate(
    group_CVLAI     = ifelse(str_detect(CV_LAI_label, "CVLAI\\+"), "CVLAI+", "CVLAI-"),
    group_CVIAVppt  = ifelse(str_detect(CV_LAI_label, "CVIAVppt\\+"), "CVIAVppt+", "CVIAVppt-"),
    group_LAIppt    = ifelse(str_detect(CV_LAI_label, "LAIppt_m\\+"), "LAIppt_m+", "LAIppt_m-")
  )

# Merge into annual data
annual_data_lai <- annual_data %>%
  left_join(site_coords %>% st_drop_geometry() %>% select(SITE_ID, CV_LAI_label, group_CVLAI, group_CVIAVppt, group_LAIppt),
            by = c("site" = "SITE_ID"))

library(dplyr)
library(ggplot2)

# Ensure TIMESTAMP is numeric or date
annual_data_lai <- annual_data_lai %>%
  mutate(TIMESTAMP = as.numeric(TIMESTAMP))  # Or use `ymd()` if needed

# ---- Compute summary statistics ----
gpp_grouped_LAIppt <- annual_data_lai %>%
  filter(!is.na(group_LAIppt), !is.na(GPP_NT_VUT_REF)) %>%
  group_by(group_LAIppt, TIMESTAMP) %>%
  dplyr::summarise(
    mean_gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    se_gpp = sd(GPP_NT_VUT_REF, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_gpp - 1.645 * se_gpp,
    upper = mean_gpp + 1.645 * se_gpp
  )

# ---- Plot with Confidence Ribbon ----
GPP_LAI_grouped_plot <- ggplot(gpp_grouped_LAIppt, aes(x = TIMESTAMP, y = mean_gpp, color = group_LAIppt, fill = group_LAIppt)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  labs(
    title = "Mean Annual GPP by LAIppt_m Group",
    x = "Year",
    y = expression(GPP~(mu*mol~m^{-2}~s^{-1})),
    color = "Group", fill = "Group"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

# ---- Plot 2: GPP grouped by full 8-class CV_LAI category ----
gpp_grouped_CVlabel <- annual_data_lai %>%
  filter(!is.na(CV_LAI_label)) %>%
  group_by(CV_LAI_label, TIMESTAMP) %>%
  summarize(
    mean_gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    se_gpp = sd(GPP_NT_VUT_REF, na.rm = TRUE) / sqrt(sum(!is.na(GPP_NT_VUT_REF))),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_gpp - 1.645 * se_gpp,
    upper = mean_gpp + 1.645 * se_gpp
  )

GPP_LAIchanges_plot <- ggplot(gpp_grouped_CVlabel, aes(x = TIMESTAMP, y = mean_gpp, color = CV_LAI_label, fill = CV_LAI_label)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  labs(
    title = "Mean Annual GPP Over Time by CV_LAI Condition",
    x = "Year",
    y = expression(GPP~(mu*mol~m^{-2}~s^{-1})),
    color = "CV_LAI Category", fill = "CV_LAI Category"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")



#####
#####
#####

# Ensure TIMESTAMP is numeric or date
annual_data_lai <- annual_data_lai %>%
  mutate(
    TIMESTAMP = as.numeric(TIMESTAMP),     # or use ymd() if TIMESTAMP is date-formatted
    is_US = str_starts(site, "US")
  )
# ---- Compute Summary Statistics: US Sites Only ----
gpp_grouped_US <- annual_data_lai %>%
  filter(is_US, !is.na(group_LAIppt), !is.na(GPP_NT_VUT_REF), !is.na(IGBP)) %>%
  group_by(group_LAIppt, TIMESTAMP, IGBP) %>%
  dplyr::summarise(
    mean_gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    se_gpp = sd(GPP_NT_VUT_REF, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_gpp - 1.645 * se_gpp,
    upper = mean_gpp + 1.645 * se_gpp
  )


unique_igbp <- sort(unique(gpp_grouped_US$IGBP))

plots_US_IGBP <- list()

for (igbp in unique_igbp) {
  p <- gpp_grouped_US %>%
    filter(IGBP == igbp) %>%
    ggplot(aes(x = TIMESTAMP, y = mean_gpp, color = group_LAIppt, fill = group_LAIppt)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    labs(
      title = paste("Mean Annual GPP for", igbp, "(US Sites Only)"),
      x = "Year",
      y = expression(GPP~(mu*mol~m^{-2}~s^{-1})),
      color = "Group", fill = "Group"
    ) +
    theme_classic(base_size = 14) +
    theme(legend.position = "bottom")
  
  plots_US_IGBP[[igbp]] <- p
}

print(plots_US_IGBP)





# Save the plot
ggsave("saved_plots/GPP_by_CV_LAI_category.png", GPP_LAIchanges, width = 10, height = 6, dpi = 300)