# fluxnet_workflow_refactor.R
# Refactored FLUXNET workflow for daily and annual analysis
# Authors: Kristina Riemer & Dave Moore (refactored by ChatGPT)

# Load libraries
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(amerifluxr)
library(ggnewscale)
library(forcats)


# ------------------------
# Example Usage
# ------------------------
analysis_mode <- "annual"

# ------------------------
# Configuration
# ------------------------
config <- list(
  daily_cache = "data/multiple_sites_daily.rds",
  annual_cache = "data/multiple_sites_annual.rds",
  columns_to_clean = c("GPP_NT_VUT_REF", "RECO_NT_VUT_REF", "NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")
)

# ------------------------
# Utility functions
# ------------------------
extract_site <- function(path) {
  str_split(basename(path), "_", simplify = TRUE)[, 2]
}

load_and_cache <- function(patterns, cache_file, extract_site_func) {
  paths <- list.files(".", pattern = paste(patterns, collapse = "|"), recursive = TRUE, full.names = TRUE)
  paths <- paths[!grepl("VARINFO", basename(paths))]
  
  if (file.exists(cache_file)) {
    data <- readRDS(cache_file)
    new_paths <- paths[!extract_site_func(paths) %in% unique(data$site)]
    if (length(new_paths) > 0) {
      new_data <- map_df(new_paths, ~ read_csv(.x) %>%
                           mutate(file = basename(.x), .before = 1) %>%
                           mutate(site = extract_site_func(.x)))
      data <- bind_rows(data, new_data)
      saveRDS(data, cache_file)
    }
  } else {
    data <- map_df(paths, ~ read_csv(.x) %>%
                     mutate(file = basename(.x), .before = 1) %>%
                     mutate(site = extract_site_func(.x)))
    saveRDS(data, cache_file)
  }
  return(data)
}

clean_fluxnet_data <- function(df, columns) {
  df %>% mutate(across(all_of(columns), ~ ifelse(. < -9000, NA, .)))
}

add_site_metadata <- function(data, metadata) {
  left_join(data, metadata, by = c("site" = "SITE_ID"))
}

# ------------------------
# Metadata loading
# ------------------------
af_meta <- amf_site_info() %>%
  select(SITE_ID, SITE_NAME, COUNTRY, STATE, IGBP,
         LOCATION_LAT, LOCATION_LONG, LOCATION_ELEV,
         CLIMATE_KOEPPEN, MAT, MAP) %>%
  mutate(DATA_SOURCE = "AmeriFlux")

icos_files <- list.files("data", pattern = "ICOSETC_.*_SITEINFO_L2\\.csv$", recursive = TRUE, full.names = TRUE)

icos_meta <- map_dfr(icos_files, function(path) {
  read_csv(path, col_types = cols(SITE_ID = col_character(), GROUP_ID = col_character(),
                                  VARIABLE = col_character(), DATAVALUE = col_character())) %>%
    select(SITE_ID, VARIABLE, DATAVALUE) %>%
    group_by(SITE_ID, VARIABLE) %>%
    summarize(DATAVALUE = first(DATAVALUE), .groups = "drop") %>%
    pivot_wider(names_from = VARIABLE, values_from = DATAVALUE)
})

icos_meta_clean <- icos_meta %>%
  transmute(
    SITE_ID,
    SITE_NAME,
    COUNTRY = NA_character_,
    STATE = NA_character_,
    IGBP,
    LOCATION_LAT = as.numeric(LOCATION_LAT),
    LOCATION_LONG = as.numeric(LOCATION_LONG),
    LOCATION_ELEV = as.numeric(LOCATION_ELEV),
    CLIMATE_KOEPPEN,
    MAT = as.numeric(MAT),
    MAP = as.numeric(MAP),
    DATA_SOURCE = "ICOS"
  )

site_metadata <- bind_rows(af_meta, icos_meta_clean) %>%
  distinct(SITE_ID, .keep_all = TRUE)

# ------------------------
# Data Loaders
# ------------------------
load_and_clean_daily_data <- function() {
  patterns <- c("FLUXNET_FULLSET_DD.*\\.csv$", "ICOSETC_[^/]+_FLUXNET_DD_L2\\.csv$")
  daily_data <- load_and_cache(patterns, config$daily_cache, extract_site)
  daily_data <- clean_fluxnet_data(daily_data, config$columns_to_clean)
  daily_data <- add_site_metadata(daily_data, site_metadata)
  daily_data <- daily_data %>% mutate(date_object = ymd(TIMESTAMP))
  return(daily_data)
}

load_and_clean_annual_data <- function() {
  patterns <- c("FLUXNET_FULLSET_YY.*\\.csv$", "ICOSETC_[^/]+_FLUXNET_YY_L2\\.csv$")
  annual_data <- load_and_cache(patterns, config$annual_cache, extract_site)
  annual_data <- clean_fluxnet_data(annual_data, config$columns_to_clean)
  annual_data <- add_site_metadata(annual_data, site_metadata)
  annual_data <- annual_data %>% mutate(year = TIMESTAMP)
  return(annual_data)
}

if (analysis_mode == "daily") {
  daily_data <- load_and_clean_daily_data()
  # call daily plotting functions
} else {
  annual_data <- load_and_clean_annual_data()
}





# ------------------------
# Annual Plotting Functions
# ------------------------
plot_annual_fluxnet_data <- function(annual_data) {
  p1 <- ggplot(annual_data, aes(x = P_F, y = NEE_VUT_REF, color = IGBP)) +
    geom_point(alpha = 0.6) +
    labs(x = "Annual Precipitation (mm)", y = "Annual NEE") +
    theme_minimal()
  
  p2 <- ggplot(annual_data, aes(x = TA_F, y = GPP_NT_VUT_REF, color = IGBP)) +
    geom_point(alpha = 0.6) +
    labs(x = "Annual Temperature (°C)", y = "Annual GPP") +
    theme_minimal()
  
  return(list(precip_vs_nee = p1, temp_vs_gpp = p2))
}

plot_flux_box_by_group <- function(annual_data, flux_var = "NEE_VUT_REF", y_mode = "full") {
  # Define IGBP groups
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  
  # Optionally calculate WUE
  if (flux_var == "WUE") {
    annual_data <- annual_data %>%
      mutate(FLUX = GPP_NT_VUT_REF / LE_F_MDS)
  } else {
    annual_data <- annual_data %>%
      mutate(FLUX = .data[[flux_var]])
  }
  
  # Clean and group
  annual_data <- annual_data %>%
    mutate(
      year = TIMESTAMP,
      flux_sign = factor(
        ifelse(FLUX < 0, "Negative", "Positive"),
        levels = c("Negative", "Positive")
      ),
      IGBP_group = case_when(
        IGBP %in% group1 ~ "Forest",
        IGBP %in% group2 ~ "Shrub/Opens",
        IGBP %in% group3 ~ "Grass/Crops/Wet",
        TRUE ~ "Other"
      )
    )
  
  # Site count annotations
  site_counts <- annual_data %>%
    group_by(year, IGBP) %>%
    summarize(n_sites = n_distinct(site), .groups = "drop")
  
  # Label formatting
  y_label <- case_when(
    flux_var == "NEE_VUT_REF" ~ "NEE (μmol m⁻² s⁻¹)",
    flux_var == "GPP_NT_VUT_REF" ~ "GPP (μmol m⁻² s⁻¹)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (μmol m⁻² s⁻¹)",
    flux_var == "LE_F_MDS" ~ "LE (W m⁻²)",
    flux_var == "WUE" ~ "Water Use Efficiency (GPP / LE)",
    TRUE ~ flux_var
  )
  
  plot_box <- function(data, counts, label) {
    # Y-axis configuration
    y_limits <- if (y_mode == "squish") {
      upper <- quantile(data$FLUX, probs = 0.95, na.rm = TRUE)
      scale_y_continuous(limits = c(0, upper), oob = scales::squish)
    } else {
      scale_y_continuous()
    }
    
    ggplot(data, aes(x = factor(year), y = FLUX)) +
      geom_boxplot(outlier.shape = NA, fill = NA, color = "black", alpha = 0.4) +
      geom_jitter(aes(color = flux_sign), size = 0.8, alpha = 0.3, width = 0.25) +
      geom_text(
        data = counts,
        aes(x = factor(year), y = max(data$FLUX, na.rm = TRUE) * 1.05, label = paste0("n=", n_sites)),
        inherit.aes = FALSE,
        size = 3, vjust = 0
      ) +
      scale_color_manual(
        values = c("Negative" = "#1b9e77", "Positive" = "#d95f02"),
        name = paste0(label, " Sign")
      ) +
      y_limits +
      facet_wrap(vars(IGBP), ncol = 1, strip.position = "right") +
      labs(title = label, x = "Year", y = y_label) +
      theme_classic() +
      theme(
        panel.background = element_rect(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
  
  grouped_data <- function(label) {
    filter(annual_data, IGBP_group == label)
  }
  
  grouped_counts <- function(label) {
    filter(site_counts, IGBP %in% unique(grouped_data(label)$IGBP))
  }
  
  list(
    Forest = plot_box(grouped_data("Forest"), grouped_counts("Forest"), "Forest"),
    ShrubOpens = plot_box(grouped_data("Shrub/Opens"), grouped_counts("Shrub/Opens"), "Shrub/Opens"),
    GrassCropsWet = plot_box(grouped_data("Grass/Crops/Wet"), grouped_counts("Grass/Crops/Wet"), "Grass/Crops/Wet"),
    Other = plot_box(grouped_data("Other"), grouped_counts("Other"), "Other")
  )
}


interannual_climate_summary <- function(annual_data) {
  annual_data %>%
    group_by(site) %>%
    summarize(
      min_precip = min(P_F, na.rm = TRUE),
      max_precip = max(P_F, na.rm = TRUE),
      min_temp = min(TA_F, na.rm = TRUE),
      max_temp = max(TA_F, na.rm = TRUE),
      min_nee = min(NEE_VUT_REF, na.rm = TRUE),
      max_nee = max(NEE_VUT_REF, na.rm = TRUE),
      .groups = "drop"
    )
}

generate_whittaker_points <- function(annual_data, site_metadata) {
  annual_data %>%
    group_by(site) %>%
    summarize(mean_precip = mean(P_F, na.rm = TRUE),
              mean_temp = mean(TA_F, na.rm = TRUE),
              mean_nee = mean(NEE_VUT_REF, na.rm = TRUE), .groups = "drop") %>%
    mutate(mean_precip_cm = mean_precip / 10,
           NEE_sign = ifelse(mean_nee < 0, "Sink", "Source")) %>%
    left_join(site_metadata, by = c("site" = "SITE_ID"))
}


plot_latitudinal_flux <- function(annual_data, site_metadata, flux_var = "NEE_VUT_REF", bin_width = 5, shape_palette = 0:15) {
  # Optionally filter out negative GPP values
  if (flux_var == "GPP_NT_VUT_REF") {
    annual_data <- annual_data %>%
      filter(.data[[flux_var]] >= 0)
  }
  
  # Step 1: Summarize flux per site
  site_summary <- annual_data %>%
    group_by(site) %>%
    summarize(
      min_flux  = min(.data[[flux_var]], na.rm = TRUE),
      max_flux  = max(.data[[flux_var]], na.rm = TRUE),
      mean_flux = mean(.data[[flux_var]], na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    left_join(site_metadata %>% select(SITE_ID, LOCATION_LAT, IGBP), by = c("site" = "SITE_ID")) %>%
    rename(lat = LOCATION_LAT)
  
  # Step 2: Bin sites by latitude
  breaks_vec <- seq(-90, 90, by = bin_width)
  binned_summary <- site_summary %>%
    mutate(
      lat_bin = cut(lat, breaks = breaks_vec, include.lowest = TRUE, right = FALSE)
    ) %>%
    group_by(lat_bin) %>%
    summarize(
      lat_lower = first(breaks_vec[as.integer(lat_bin)]),
      lat_upper = lat_lower + bin_width,
      min_flux  = min(min_flux, na.rm = TRUE),
      max_flux  = max(max_flux, na.rm = TRUE),
      .groups   = "drop"
    )
  
  # Create a y-axis label and title based on flux_var
  y_label <- case_when(
    flux_var == "NEE_VUT_REF" ~ "NEE (μmol m⁻² s⁻¹)",
    flux_var == "GPP_NT_VUT_REF" ~ "GPP (μmol m⁻² s⁻¹)",
    flux_var == "LE_F_MDS" ~ "LE (W m⁻²)",
    TRUE ~ flux_var
  )
  
  title_text <- paste0("Annual ", y_label, " range in ", bin_width, "° lat bands")
  
  # Step 3: Generate the plot
  ggplot() +
    geom_ribbon(
      data = binned_summary,
      aes(
        x = lat_lower + bin_width / 2,
        ymin = min_flux,
        ymax = max_flux
      ),
      fill = "steelblue",
      alpha = 0.4
    ) +
    coord_flip() +
    geom_point(
      data = site_summary,
      aes(x = lat, y = mean_flux, shape = IGBP),
      size = 3,
      color = "black",
      stroke = 0.8,
      inherit.aes = FALSE
    ) +
    scale_shape_manual(values = shape_palette) +
    labs(
      x = "Latitude (°)",
      y = y_label,
      title = title_text
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 10),
      panel.background = element_rect(color = "black"),
      legend.position = "right"
    )
}


# New scatterplot function
PlotXY_annual <- function(annual_data, x_var, y_var, shape_palette = 0:15) {
  ggplot(annual_data, aes(x = .data[[x_var]], y = .data[[y_var]], shape = IGBP)) +
    geom_point(size = 2.5, stroke = 0.7, color = "black") +
    scale_shape_manual(values = shape_palette) +
    labs(
      x = x_var,
      y = y_var,
      shape = "IGBP"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(color = "black"),
      legend.position = "right"
    )
}

# ------------------------
# Example Usage
# ------------------------
analysis_mode <- "annual"
if (analysis_mode == "daily") {
  daily_data <- load_and_clean_daily_data()
  # call daily plotting functions
} else {
  annual_data <- load_and_clean_annual_data()
}


# ------------------------
# 📘 Usage Notes: Annual Workflow & Plotting
# ------------------------
# 
# 1. Load and Prepare Annual Data:
#    annual_data <- load_and_clean_annual_data()
#    - Loads and caches annual FLUXNET data.
#    - Cleans unrealistic values (< -9000).
#    - Joins with site metadata (IGBP, lat/lon, etc.).
#
# 2. Plotting Options:
#
#    A. Climate vs Flux Summary
#       plots <- plot_annual_fluxnet_data(annual_data)
#       print(plots$precip_vs_nee)
#       print(plots$temp_vs_gpp)
#       > Scatterplots colored by IGBP.
#
#    B. Latitudinal Summary by Flux
#       plot_latitudinal_flux(annual_data, site_metadata, flux_var = "GPP_NT_VUT_REF")
#       > Options for NEE, GPP, LE. Negative GPP values are filtered.
#       > Bin width = 5° by default.
#
#    C. Interannual Boxplots by IGBP Group
#       boxplots_full   <- plot_flux_box_by_group(annual_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")
#       boxplots_squish <- plot_flux_box_by_group(annual_data, flux_var = "WUE", y_mode = "squish")
#       > flux_var options: NEE, GPP, RECO, LE, WUE
#       > y_mode = "full" or "squish" (95th percentile)
#       > IGBP groups: Forest, Shrub/Opens, Grass/Crops/Wet, Other
#
#    D. Custom XY Scatterplots
#       PlotXY_annual(annual_data, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS")
#       > Allows any two annual variables.
#       > IGBP shown with distinct shapes.
#
# 3. Derived Summary Functions:
#    interannual_climate_summary(annual_data)
#    generate_whittaker_points(annual_data, site_metadata)
#    > Generate site-level summaries and Whittaker diagram-ready points
#
# ------------------------------------------------------------------

plots <- plot_annual_fluxnet_data(annual_data) 
boxplots <- plot_nee_box_by_group(annual_data)
print(plots$precip_vs_nee)
# For NEE
plot_latitudinal_flux(annual_data, site_metadata, flux_var = "NEE_VUT_REF")

# Full range
boxplots_full <- plot_flux_box_by_group(annual_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")
# Squished top 5%
boxplots_squish <- plot_flux_box_by_group(annual_data, flux_var = "WUE", y_mode = "squish")


PlotXY_annual(annual_data, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS")
PlotXY_annual(annual_data, x_var = "NEE_VUT_REF", y_var = "LE_F_MDS")


# ##############################
# Daily data analysis and plotting
# ##############################

# ------------------------
# Daily Plot: Seasonal Climatology by IGBP Group
# ------------------------
# ------------------------
# Daily Plot: Seasonal Climatology by IGBP Group
# ------------------------

# ------------------------
# Daily Plot: Seasonal Climatology by IGBP Group
# ------------------------

# ------------------------
# Daily Plot: Seasonal Climatology by IGBP Group
# ------------------------

# ------------------------
# Daily Plot: Seasonal Climatology by IGBP Group
# ------------------------

plot_seasonal_cycle <- function(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full") {
  # Group definitions (reuse from annual boxplot)
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  
  daily_data <- daily_data %>%
    mutate(
      DOY = yday(date_object),
      FLUX = ifelse(.data[[flux_var]] < -9000, NA, .data[[flux_var]]),
      IGBP_group = case_when(
        IGBP %in% group1 ~ "Forest",
        IGBP %in% group2 ~ "Shrub/Opens",
        IGBP %in% group3 ~ "Grass/Crops/Wet",
        TRUE ~ "Other"
      )
    )
  
  y_label <- case_when(
    flux_var == "NEE_VUT_REF" ~ "NEE (μmol m-2 s-1)",
    flux_var == "GPP_NT_VUT_REF" ~ "GPP (μmol m-2 s-1)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (μmol m-2 s-1)",
    flux_var == "LE_F_MDS" ~ "LE (W m-2)",
    TRUE ~ flux_var
  )
  
  
  y_scale_func <- function(data) {
    if (y_mode == "squish") {
      upper <- quantile(data$mean_flux, probs = 0.95, na.rm = TRUE)
      scale_y_continuous(limits = c(0, upper), oob = scales::squish)
    } else {
      scale_y_continuous()
    }
  }
  
  plot_group <- function(data, group_label) {
    group_data <- data %>% filter(IGBP_group == group_label)
    seasonal_summary <- group_data %>%
      group_by(IGBP, DOY) %>%
      summarize(
        mean_flux = mean(FLUX, na.rm = TRUE),
        se_flux = sd(FLUX, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(
        ci_lower = mean_flux - qt(0.975, df = n() - 1) * se_flux,
        ci_upper = mean_flux + qt(0.975, df = n() - 1) * se_flux
      )
    
    site_stats <- group_data %>%
      filter(!is.na(FLUX)) %>%
      group_by(site, IGBP) %>%
      summarize(n_years = n_distinct(year(date_object)), .groups = "drop")
    
    igbp_labels <- site_stats %>%
      group_by(IGBP) %>%
      summarize(
        n_sites = n_distinct(site),
        site_years = sum(n_years), .groups = "drop"
      ) %>%
      mutate(
        label = paste0(IGBP, ": ", n_sites, " sites, ", site_years, " site-years")
      )
    
    y_max <- max(seasonal_summary$ci_upper, na.rm = TRUE)
    y_min <- min(seasonal_summary$ci_lower, na.rm = TRUE)
    spacing <- (y_max - y_min) * 0.05
    igbp_labels <- igbp_labels %>%
      arrange(desc(IGBP)) %>%
      mutate(
        x = 365,
        y = y_max - (row_number() - 1) * spacing
      )
    
    ggplot(seasonal_summary, aes(x = DOY, y = mean_flux, color = IGBP, fill = IGBP)) +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1.1) +
      y_scale_func(seasonal_summary) +
      geom_text(
        data = igbp_labels,
        aes(x = x, y = y, label = label, color = IGBP),
        hjust = 1.1, vjust = 1, inherit.aes = FALSE, size = 3
      ) +
      labs(
        x = "Day of Year",
        y = y_label,
        title = paste("Seasonal Cycle of", y_label, "for", group_label),
        color = "IGBP",
        fill = "IGBP"
      ) +
      theme_classic(base_size = 14) +
      theme(
        panel.background = element_rect(color = "black"),
        legend.position = "right"
      )
  }
  
  list(
    Forest = plot_group(daily_data, "Forest"),
    ShrubOpens = plot_group(daily_data, "Shrub/Opens"),
    GrassCropsWet = plot_group(daily_data, "Grass/Crops/Wet"),
    Other = plot_group(daily_data, "Other")
  )
}

# ------------------------
# Phenology Detection via Integral Smoothing
# ------------------------

#' detect_phenology_integral: Estimate phenological transition dates via integral smoothing
#'
#' @param daily_data A dataframe with daily time series data including a 'site', 'date_object', and flux variable (e.g., GPP).
#' @param knots Number of knots for spline smoothing (default = 10).
#' @param flux_var Flux variable to analyze, typically "GPP_NT_VUT_REF".
#' @return A list containing:
#'   - phenology_df: A dataframe with columns site, year, SOS (start of season), POS (peak of season), and EOS (end of season).
#'   - issues_df: A dataframe logging problematic records (e.g., SOS = 1, missing values, or temporal ordering issues).
#'
#' @details This method uses a cumulative-sum-based smoothing technique. The data is padded with ±30 days on either end of each year,
#' spline-smoothed, and differentiated to obtain a smooth GPP curve. SOS and EOS are based on 20% of the max smoothed value, and POS is the day of peak.
#'
#' This approach increases signal-to-noise ratio and is more robust to parameter choices than direct smoothing.
#'
#' Note: Requires 'date_object' as a Date column and assumes daily temporal resolution.


detect_phenology_integral <- function(daily_data, knots = 10, flux_var = "GPP_NT_VUT_REF") {
  library(splines)
  library(dplyr)
  
  if (!"date_object" %in% names(daily_data)) {
    stop("daily_data must contain 'date_object' column as Date")
  }
  
  daily_data <- daily_data %>%
    filter(!is.na(.data[[flux_var]])) %>%
    mutate(
      year = lubridate::year(date_object),
      DOY = lubridate::yday(date_object),
      hemisphere = ifelse(LOCATION_LAT < 0, "SH", "NH"),
      climate_zone = case_when(
        abs(LOCATION_LAT) < 23.5 ~ "Tropical",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT >= 0 ~ "Temperate_North",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT < 0 ~ "Temperate_South",
        TRUE ~ "Other"
      ),
      DOY_aligned = case_when(
        climate_zone == "Temperate_South" ~ (DOY + 182) %% 365,
        TRUE ~ DOY
      )
    )
  
  phenology_results <- list()
  issues_log <- list()
  
  unique_sites <- unique(daily_data$site)
  
  for (site_id in unique_sites) {
    site_data <- daily_data %>% filter(site == site_id)
    unique_years <- sort(unique(site_data$year))
    site_climate <- site_data$climate_zone[1]
    
    for (yr in unique_years) {
      year_data <- site_data %>% filter(year == yr)
      prev_data <- site_data %>% filter(year == yr - 1) %>% tail(30)
      next_data <- site_data %>% filter(year == yr + 1) %>% head(30)
      padded_data <- bind_rows(prev_data, year_data, next_data)
      
      padded_data <- padded_data %>%
        arrange(date_object) %>%
        mutate(cumflux = cumsum(.data[[flux_var]]))
      
      if (nrow(padded_data) < knots) next
      
      tryCatch({
        spline_fit <- smooth.spline(x = 1:nrow(padded_data), y = padded_data$cumflux, df = knots)
        deriv_vals <- predict(spline_fit, deriv = 1)$y
        
        smoothed_gpp <- tibble(
          site = site_id,
          date = padded_data$date_object,
          DOY = padded_data$DOY,
          DOY_aligned = padded_data$DOY_aligned,
          year = padded_data$year,
          smoothed_flux = deriv_vals
        ) %>% filter(year == yr)
        
        max_flux <- max(smoothed_gpp$smoothed_flux, na.rm = TRUE)
        threshold_20 <- 0.2 * max_flux
        
        sos <- smoothed_gpp %>% filter(smoothed_flux >= threshold_20) %>% slice_head(n = 1)
        pos <- smoothed_gpp %>% filter(smoothed_flux == max_flux)
        eos <- smoothed_gpp %>% filter(DOY > pos$DOY[1] & smoothed_flux <= threshold_20) %>% slice_head(n = 1)
        
        SOS_val <- sos$DOY[1]
        POS_val <- pos$DOY[1]
        EOS_val <- eos$DOY[1]
        
        # Track issues
        issue_reason <- NULL
        if (is.na(SOS_val) | is.na(POS_val) | is.na(EOS_val)) {
          issue_reason <- "NA in SOS/POS/EOS"
        } else if (SOS_val == 1) {
          issue_reason <- "SOS = 1 (possibly spurious)"
        } else if (site_climate != "Tropical" && (SOS_val >= POS_val || POS_val >= EOS_val)) {
          issue_reason <- "Order violation (SOS >= POS or POS >= EOS)"
        }
        
        if (!is.null(issue_reason)) {
          issues_log[[length(issues_log) + 1]] <- tibble(
            site = site_id,
            year = yr,
            SOS = SOS_val,
            POS = POS_val,
            EOS = EOS_val,
            reason = issue_reason
          )
        }
        
        phenology_results[[length(phenology_results) + 1]] <- tibble(
          site = site_id,
          year = yr,
          SOS = SOS_val,
          POS = POS_val,
          EOS = EOS_val
        )
      }, error = function(e) {})
    }
  }
  
  return(list(
    phenology_df = bind_rows(phenology_results),
    issues_df = bind_rows(issues_log)
  ))
}


# Time series plot of SOS, POS, EOS by IGBP group
plot_phenology_timeseries <- function(phenology_df, site_metadata) {
  phenology_df <- phenology_df %>%
    left_join(site_metadata, by = c("site" = "SITE_ID")) %>%
    mutate(
      IGBP_group = case_when(
        IGBP %in% c("DBF", "ENF", "MF", "EBF") ~ "Forest",
        IGBP %in% c("OSH", "CSH", "WSA", "SAV") ~ "Shrub/Opens",
        IGBP %in% c("GRA", "CRO", "WET") ~ "Grass/Crops/Wet",
        TRUE ~ "Other"
      )
    )
  
  phenology_long <- phenology_df %>%
    pivot_longer(cols = c(SOS, POS, EOS), names_to = "phase", values_to = "DOY")
  
  plot_list <- list()
  for (p in unique(phenology_long$phase)) {
    phase_data <- phenology_long %>% filter(phase == p)
    
    for (grp in unique(phenology_long$IGBP_group)) {
      group_data <- phase_data %>% filter(IGBP_group == grp)
      p_plot <- ggplot(group_data, aes(x = year, y = DOY, color = IGBP)) +
        geom_point(alpha = 0.5, size = 1) +
        geom_smooth(method = "loess", span = 0.3, se = FALSE) +
        facet_wrap(~ IGBP, scales = "free_y") +
        labs(
          title = paste0(p, " Timing in ", grp, " Group"),
          x = "Year",
          y = "Day of Year"
        ) +
        theme_classic(base_size = 14) +
        theme(
          panel.background = element_rect(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      plot_list[[paste(p, grp, sep = "_")]] <- p_plot
    }
  }
  return(plot_list)
}


# Wrapper to visualize smoothing process for valid site-years
illustrate_integral_smoothing_example <- function(daily_data, flux_var = "GPP_NT_VUT_REF") {
  valid_site_years <- daily_data %>%
    filter(!is.na(.data[[flux_var]])) %>%
    mutate(year = lubridate::year(date_object)) %>%
    group_by(site, year) %>%
    summarise(days_with_data = n(), .groups = "drop") %>%
    filter(days_with_data >= 300)
  
  if (nrow(valid_site_years) == 0) {
    warning("No valid site-years with sufficient data found.")
    return(NULL)
  }
  
  example <- valid_site_years %>% sample_n(1)
  illustrate_integral_smoothing(daily_data, site_id = example$site, year = example$year, flux_var = flux_var)
}


daily_data <- load_and_clean_daily_data()
plot_seasonal_cycle(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")


# # Run the phenology detection on your daily data
# phen_results <- detect_phenology_integral(
#   daily_data = daily_data,
#   knots = 10,
#   flux_var = "GPP_NT_VUT_REF"
# )


# 
# # The result is a list with two data frames:
# phenology_df <- phen_results$phenology_df
# issues_df    <- phen_results$issues_df
# 
# illustrate_integral_smoothing_example(daily_data = daily_data)
# # View successful phenology estimates
# head(phenology_df)
# 
# # View records where phenology estimation failed or was suspicious
# head(issues_df)
# 
# # Optional: filter and inspect just order violations
# issues_df %>% filter(reason == "Order violation (SOS >= POS or POS >= EOS)")
# # Plot the time series for SOS, POS, EOS grouped by IGBP type
# phen_plots <- plot_phenology_timeseries(phenology_df, site_metadata)


# Example: Show the POS timing for Forest group
print(phen_plots$EOS_Forest)




