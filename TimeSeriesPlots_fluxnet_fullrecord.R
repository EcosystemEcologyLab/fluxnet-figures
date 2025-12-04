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
library(minpack.lm) #for phenology code
library(patchwork)
library(splines)

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
  columns_to_clean = c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "RECO_NT_VUT_REF", "NEE_VUT_REF", "LE_F_MDS", "H_F_MDS", "PPFD_IN", "TA_F_MDS")
)

my_15_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
  "#999999",
  "#66C2A5", "#000070", "#8DA0CB",
  "#E78AC3", "#A6D854", "#FFD92F"
)

my_16_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
  "#999999",
  "#66C2A5", "#000070", "#8DA0CB",
  "#E78AC3", "#A6D854", "#FFD92F","#C7C7C7"
)

my_15_shapes <- c(1:15)
my_16_shapes <- c(1:16)

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

clean_fluxnet_data <- function(data, site_metadata) {
  numeric_cols <- names(select(data, where(is.numeric)))
  data_cleaned <- data %>%
    mutate(across(all_of(numeric_cols), ~ ifelse(. < -9000, NA, .))) %>%
    select(-any_of(names(site_metadata))) %>%
    left_join(site_metadata, by = c("site" = "SITE_ID"))
  return(data_cleaned)
}

add_site_metadata <- function(data, metadata) {
  data %>% select(-any_of(names(metadata))) %>%
    left_join(metadata, by = c("site" = "SITE_ID"))
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
  daily_data <- clean_fluxnet_data(daily_data, site_metadata)
  daily_data <- daily_data %>% mutate(date_object = ymd(TIMESTAMP))
  return(daily_data)
}

load_and_clean_annual_data <- function() {
  patterns <- c("FLUXNET_FULLSET_YY.*\\.csv$", "ICOSETC_[^/]+_FLUXNET_YY_L2\\.csv$")
  annual_data <- load_and_cache(patterns, config$annual_cache, extract_site)
  annual_data <- clean_fluxnet_data(annual_data, site_metadata)
  annual_data <- annual_data %>% mutate(year = TIMESTAMP)
  return(annual_data)
}

if (analysis_mode == "daily") {
  daily_data <- load_and_clean_daily_data()
  # call daily plotting functions
} else {
  annual_data <- load_and_clean_annual_data()
}

# Helper Function to Save Plot Lists
save_plot_list <- function(plot_list, prefix = "plot", out_dir = "saved_plots", width = 8, height = 6) {
  dir.create(out_dir, showWarnings = FALSE)
  for (name in names(plot_list)) {
    file_path <- file.path(out_dir, paste0(prefix, "_", name, ".png"))
    ggsave(file_path, plot_list[[name]], width = width, height = height, dpi = 300)
  }
}


# ------------------------
# Annual Plotting Functions
# ------------------------

# -----------------------------
# New: IGBP Aggregated Flux Summary
# -----------------------------
plot_flux_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF") {
  igbp_order <- c("DBF", "ENF", "MF", "DNF", "EBF", "OSH", "CSH", "WSA", "SAV", "GRA", "CRO", "WET", "BSV")
  
  annual_data <- annual_data %>%
    mutate(
      FLUX = .data[[flux_var]],
      flux_sign = factor(
        ifelse(FLUX < 0, "Negative", "Positive"),
        levels = c("Negative", "Positive")
      ),
      IGBP = factor(IGBP, levels = igbp_order)
    )
  
  y_label <- case_when(
    flux_var == "NEE_VUT_REF"     ~ "NEE (μmol m⁻² s⁻¹)",
    flux_var == "GPP_NT_VUT_REF"  ~ "GPP (μmol m⁻² s⁻¹)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (μmol m⁻² s⁻¹)",
    flux_var == "LE_F_MDS"        ~ "LE (W m⁻²)",
    flux_var == "WUE"             ~ "WUE (GPP / LE)",
    TRUE                          ~ flux_var
  )
  
  # Boxplot with jitter
  p_flux <- ggplot(annual_data, aes(x = IGBP, y = FLUX)) +
    geom_boxplot(color = "black", fill = NA, outlier.shape = NA) +
    geom_jitter(aes(color = flux_sign), width = 0.25, alpha = 0.4, size = 1) +
    scale_color_manual(
      values = c("Negative" = "#1b9e77", "Positive" = "#d95f02"),
      name = paste0(y_label, " Sign")
    ) +
    labs(
      x = NULL,
      y = y_label,
      title = paste(y_label, "Distribution by IGBP (All Years)")
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    )
  
  # Median bar plot
  summary_data <- annual_data %>%
    group_by(IGBP) %>%
    summarize(median_flux = median(FLUX, na.rm = TRUE), .groups = "drop") %>%
    mutate(IGBP = factor(IGBP, levels = igbp_order))
  
  p_median <- ggplot(summary_data, aes(x = IGBP, y = median_flux)) +
    geom_col(fill = "black", alpha = 0.6) +
    labs(
      x = NULL,
      y = "Med"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Site-year count plot
  site_counts <- annual_data %>%
    group_by(IGBP) %>%
    summarize(n_siteyears = n(), .groups = "drop") %>%
    mutate(IGBP = factor(IGBP, levels = igbp_order))
  
  p_count <- ggplot(site_counts, aes(x = IGBP, y = n_siteyears)) +
    geom_col(fill = "gray40", alpha = 0.7) +
    labs(
      x = "IGBP Class",
      y = "#SiteYrs"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.background = element_rect(color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Composite layout using patchwork
  p_composite <- p_flux / p_median / p_count +
    plot_layout(heights = c(0.7, 0.2, 0.1))
  
  list(
    flux_plot = p_flux,
    median_plot = p_median,
    count_plot = p_count,
    composite_plot = p_composite
  )
}

# Example usage:
p_flux_igbp <- plot_flux_by_igbp(annual_data, flux_var = "GPP_NT_CUT_REF")
save_plot_list(p_flux_igbp, prefix = "flux_by_IGBP_GPP")

# -----------------------------
# IGBP Aggregated Flux by Time Slice (Grouped by IGBP)
# -----------------------------
plot_flux_by_igbp_timeslice_grouped <- function(annual_data, flux_var = "NEE_VUT_REF") {
  igbp_order <- c("DBF", "ENF", "MF", "DNF", "EBF", "OSH", "CSH", "WSA", "SAV", "GRA", "CRO", "WET", "BSV")
  
  annual_data <- annual_data %>%
    mutate(
      FLUX = .data[[flux_var]],
      year = as.integer(TIMESTAMP),
      TimeSlice = cut(year,
                      breaks = seq(1999, 2025, by = 5),
                      labels = c("2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024"),
                      right = TRUE),
      IGBP = factor(IGBP, levels = igbp_order)
    ) %>%
    filter(!is.na(TimeSlice), !is.na(IGBP))
  
  y_label <- case_when(
    flux_var == "NEE_VUT_REF"     ~ "NEE (μmol m⁻² s⁻¹)",
    flux_var == "GPP_NT_VUT_REF"  ~ "GPP (μmol m⁻² s⁻¹)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (μmol m⁻² s⁻¹)",
    flux_var == "LE_F_MDS"        ~ "LE (W m⁻²)",
    flux_var == "WUE"             ~ "WUE (GPP / LE)",
    TRUE                          ~ flux_var
  )
  
  p_flux <- ggplot(annual_data, aes(x = IGBP, y = FLUX, fill = TimeSlice)) +
    geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75)) +
    labs(
      title = paste(y_label, "by IGBP and Time Slice"),
      x = "IGBP Class",
      y = y_label,
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  summary_data <- annual_data %>%
    group_by(IGBP, TimeSlice) %>%
    summarize(median_flux = median(FLUX, na.rm = TRUE), .groups = "drop")
  
  p_median <- ggplot(summary_data, aes(x = IGBP, y = median_flux, fill = TimeSlice)) +
    geom_col(position = position_dodge(width = 0.75)) +
    labs(
      title = "Median Flux by IGBP and Time Slice",
      x = "IGBP Class",
      y = "Median",
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  site_counts <- annual_data %>%
    group_by(IGBP, TimeSlice) %>%
    summarize(n_siteyears = n(), .groups = "drop")
  
  p_count <- ggplot(site_counts, aes(x = IGBP, y = n_siteyears, fill = TimeSlice)) +
    geom_col(position = position_dodge(width = 0.75)) +
    labs(
      title = "Site Years by IGBP and Time Slice",
      x = "IGBP Class",
      y = "# Site Years",
      fill = "Time Slice"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(color = "black")
    )
  
  return(list(
    flux_plot = p_flux,
    median_plot = p_median,
    count_plot = p_count
  ))
}

p_flux_igbp_time <- plot_flux_by_igbp_timeslice_grouped(annual_data, flux_var = "GPP_NT_CUT_REF")
save_plot_list(p_flux_igbp_time, prefix = "flux_by_IGBP_GPPTime")


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



#' plot_flux_timeseries_by_igbp: Plot median annual fluxes with 95% CI by IGBP over time
#'
#' @param annual_data A data frame containing annual flux summaries with columns 'site', 'TIMESTAMP', and flux variables.
#' @param flux_var The name of the flux variable to plot. Options: 'NEE_VUT_REF', 'GPP_NT_VUT_REF', 'RECO_NT_VUT_REF', 'LE_F_MDS', or 'WUE'.
#'
#' @return A ggplot object showing median flux and 95% CI over time, faceted by IGBP class.
plot_flux_timeseries_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF") {
  library(dplyr)
  library(ggplot2)
  
  # Define IGBP groupings and order
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  igbp_levels <- c(group1, group2, group3)
  
  # Add FLUX column and reorder IGBP
  annual_data <- annual_data %>%
    mutate(
      FLUX = if (flux_var == "WUE") GPP_NT_VUT_REF / LE_F_MDS else .data[[flux_var]],
      year = as.numeric(TIMESTAMP),
      IGBP = factor(IGBP, levels = igbp_levels)  # control facet order
    )
  
  # Compute summary stats
  ts_summary <- annual_data %>%
    group_by(IGBP, year) %>%
    summarise(
      median_flux = median(FLUX, na.rm = TRUE),
      lower_CI    = quantile(FLUX, 0.025, na.rm = TRUE),
      upper_CI    = quantile(FLUX, 0.975, na.rm = TRUE),
      .groups     = "drop"
    )
  
  # Y-axis label
  y_label <- case_when(
    flux_var == "NEE_VUT_REF"     ~ "NEE (µmol m-2 s-1)",
    flux_var == "GPP_NT_VUT_REF"  ~ "GPP (µmol m-2 s-1)",
    flux_var == "RECO_NT_VUT_REF" ~ "Reco (µmol m-2 s-1)",
    flux_var == "LE_F_MDS"        ~ "LE (W m-2)",
    flux_var == "WUE"             ~ "WUE (GPP / LE)",
    TRUE                          ~ flux_var
  )
  
  # Plot with ordered facets
  ggplot(ts_summary, aes(x = year, y = median_flux)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI),
                fill = "steelblue", alpha = 0.3) +
    geom_line(color = "steelblue", size = 1) +
    facet_wrap(~ IGBP, scales = "free_y", ncol = 4) +
    labs(
      x = "Year",
      y = y_label,
      title = paste("Median", y_label, "\u00b195% CI by Year & IGBP")
    ) +
    theme_classic(base_size = 14)
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
boxplots <- plot_flux_box_by_group(annual_data)
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
# WorldClim Comparison: Temperature and Precipitation
# ------------------------

compare_worldclim_siteclimate <- function(annual_data, site_metadata) {
  library(ggplot2)
  library(dplyr)
  library(terra)
  
  wc <- readRDS("data/wc_worldclim_30s.rds")
  
  site_summary <- annual_data %>%
    group_by(site) %>%
    summarise(
      MAT = mean(TA_F, na.rm = TRUE),
      MAP = mean(P_F, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(site_metadata, by = c("site" = "SITE_ID"))
  
  site_pts <- terra::vect(site_summary, geom = c("LOCATION_LONG", "LOCATION_LAT"), crs = "EPSG:4326")
  wc_extract <- terra::extract(wc[[c(1, 12)]], site_pts)
  
  site_summary$MAT_WorldClim <- wc_extract[["wc2.1_30s_bio_1"]] / 10
  site_summary$MAP_WorldClim <- wc_extract[["wc2.1_30s_bio_12"]]
  
  my_16_colors <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
    "#999999", "#66C2A5", "#000070", "#8DA0CB",
    "#E78AC3", "#A6D854", "#FFD92F", "#C7C7C7"
  )
  
  mat_plot <- ggplot(site_summary, aes(x = MAT, y = MAT_WorldClim)) +
    geom_point(aes(color = IGBP), size = 3, alpha = 0.7) +
    scale_color_manual(values = my_16_colors) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Site Mean Annual Temperature (°C)",
      y = "WorldClim MAT (°C)",
      title = "Site vs WorldClim Mean Annual Temperature"
    ) +
    theme_classic()
  
  map_plot <- ggplot(site_summary, aes(x = MAP, y = MAP_WorldClim)) +
    geom_point(aes(color = IGBP), size = 3, alpha = 0.7) +
    scale_color_manual(values = my_16_colors) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      x = "Site Mean Annual Precipitation (mm)",
      y = "WorldClim MAP (mm)",
      title = "Site vs WorldClim Mean Annual Precipitation"
    ) +
    theme_classic()
  
  summary_stats <- site_summary %>%
    summarise(
      cor_MAT = cor(MAT, MAT_WorldClim, use = "complete.obs"),
      cor_MAP = cor(MAP, MAP_WorldClim, use = "complete.obs")
    )
  
  return(list(
    mat_plot = mat_plot,
    map_plot = map_plot,
    summary = summary_stats
  ))
}

# ------------------------
# Phenology Detection via Integral Smoothing
# ------------------------

source("R/detect_phenology_integral.R")

illustrate_integral_smoothing_example <- function(daily_data, flux_var = "GPP_NT_VUT_REF", site_id = NULL, year = NULL) {
  if (!is.null(site_id) & !is.null(year)) {
    return(illustrate_integral_smoothing(daily_data, site_id = site_id, year = year, flux_var = flux_var))
  }
  
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

illustrate_integral_smoothing <- function(daily_data, site_id, year, flux_var = "GPP_NT_VUT_REF") {
  padded_data <- daily_data %>%
    filter(site == site_id & lubridate::year(date_object) %in% c(year - 1, year, year + 1)) %>%
    arrange(date_object) %>%
    filter(!is.na(.data[[flux_var]]))
  
  if (nrow(padded_data) < 10) {
    stop("Insufficient non-NA data points for spline fitting.")
  }
  
  cumflux <- cumsum(padded_data[[flux_var]])
  spline_fit <- smooth.spline(x = 1:length(cumflux), y = cumflux, df = 10)
  deriv_vals <- predict(spline_fit, deriv = 1)$y
  
  smoothed_gpp <- tibble(
    date = padded_data$date_object,
    flux = padded_data[[flux_var]],
    cumflux = cumflux,
    smoothed_flux = deriv_vals
  )
  
  # Slice out window centered on year with ±30 day buffer
  year_start <- as.Date(paste0(year, "-01-01")) - 30
  year_end <- as.Date(paste0(year, "-12-31")) + 30
  smoothed_gpp <- smoothed_gpp %>% filter(date >= year_start & date <= year_end)
  
  max_flux <- max(smoothed_gpp$smoothed_flux, na.rm = TRUE)
  threshold_20 <- 0.2 * max_flux
  
  sos <- smoothed_gpp %>% filter(smoothed_flux >= threshold_20) %>% slice_head(n = 1)
  pos <- smoothed_gpp %>% filter(smoothed_flux == max_flux)
  eos <- smoothed_gpp %>% filter(date > pos$date[1] & smoothed_flux <= threshold_20) %>% slice_head(n = 1)
  
  ggplot(smoothed_gpp, aes(x = date)) +
    geom_line(aes(y = flux), color = "gray70", size = 0.7, alpha = 0.8) +
    geom_line(aes(y = smoothed_flux), color = "steelblue", size = 1) +
    geom_vline(xintercept = sos$date, color = "green", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = pos$date, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = eos$date, color = "orange", linetype = "dashed", linewidth = 1) +
    labs(
      title = paste("Phenology Detection -", site_id, year),
      y = paste("Flux (", flux_var, ")"),
      x = "Date"
    ) +
    theme_classic(base_size = 14)
}


# ------------------------
# Phenology Timeseries and Metric Scatter Visualization
# ------------------------

plot_phenology_timeseries <- function(phenology_df, site_metadata, metric = c("LOS", "SOS", "POS", "EOS"), comparison_var = "MAT") {
  library(dplyr)
  library(ggplot2)
  metric <- match.arg(metric)
  my_16_colors <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
    "#999999", "#66C2A5", "#000070", "#8DA0CB",
    "#E78AC3", "#A6D854", "#FFD92F", "#C7C7C7"
  )
  
  group1 <- c("DBF", "ENF", "MF", "EBF")
  group2 <- c("OSH", "CSH", "WSA", "SAV")
  group3 <- c("GRA", "CRO", "WET")
  
  phen_clean <- phenology_df %>%
    left_join(site_metadata, by = c("site" = "SITE_ID")) %>%
    mutate(LOS = EOS - SOS) %>%
    filter(is.na(issues) | issues == "keep") %>%
    mutate(IGBP_group = case_when(
      IGBP %in% group1 ~ "1_Forest",
      IGBP %in% group2 ~ "2_Shrub/Opens",
      IGBP %in% group3 ~ "3_Grass/Crops/Wet",
      TRUE ~ "4_Other"
    ))
  
  ts_summary <- phen_clean %>%
    group_by(IGBP, IGBP_group, year) %>%
    summarise(
      median_value = median(.data[[metric]], na.rm = TRUE),
      lower_CI     = quantile(.data[[metric]], 0.025, na.rm = TRUE),
      upper_CI     = quantile(.data[[metric]], 0.975, na.rm = TRUE),
      .groups      = "drop"
    )
  
  p1 <- ggplot(ts_summary, aes(x = year, y = median_value)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "steelblue", alpha = 0.3) +
    geom_line(color = "steelblue", linewidth = 1) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    facet_wrap(~ reorder(IGBP, IGBP_group), scales = "free_y", ncol = 4) +
    labs(x = "Year", y = metric, title = paste("Median", metric, "±95% CI by Year & IGBP")) +
    theme_classic(base_size = 14)
  
  p2 <- ggplot(phen_clean, aes(x = factor(year), y = .data[[metric]])) +
    geom_boxplot(outlier.shape = NA, fill = "lightgray", color = "black", alpha = 0.6) +
    geom_jitter(width = 0.2, size = 0.8, alpha = 0.5) +
    scale_x_discrete(breaks = function(x) x[as.integer(x) %% 5 == 0]) +
    facet_wrap(~ reorder(IGBP, IGBP_group), scales = "free_y", ncol = 4) +
    labs(x = "Year", y = metric, title = paste(metric, "Distribution by Year & IGBP")) +
    theme_classic(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (!comparison_var %in% names(phen_clean)) {
    warning(paste("Comparison variable", comparison_var, "not found in merged dataset."))
    p3 <- NULL
  } else {
    phen_clean <- phen_clean %>% filter(!is.na(.data[[comparison_var]]))
    p3 <- ggplot(phen_clean, aes(x = .data[[comparison_var]], y = .data[[metric]])) +
      geom_point(aes(color = IGBP), size = 2, alpha = 0.7) +
      scale_color_manual(values = my_16_colors) +
      labs(
        x = comparison_var,
        y = metric,
        title = paste(metric, "vs", comparison_var)
      ) +
      theme_minimal()
  }
  
  return(list(time_series_plot = p1, boxplot = p2, scatterplot = p3))
}




# This is in development - I 'm trying to replicate an analysis from Bowling et al 2024 
# https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2023JG007839
# However it relies on hourly data and I was trying to fudge it with daily data. 
# detect_phenology_light_response <- function(daily_data, 
#                                             par_var = "PPFD_IN", 
#                                             nee_var = "NEE_VUT_REF", 
#                                             night_threshold = 20,
#                                             site_list = NULL) {
#   library(dplyr)
#   library(lubridate)
#   library(tidyr)
#   library(minpack.lm)
#   library(purrr)
#   
#   # Ensure required columns exist
#   required_vars <- c("site", "date_object", "LOCATION_LAT", nee_var, par_var)
#   stopifnot(all(required_vars %in% names(daily_data)))
#   
#   # Filter to selected sites if site_list is provided
#   if (!is.null(site_list)) {
#     daily_data <- daily_data %>% filter(site %in% site_list)
#   }
#   
#   daily_data <- daily_data %>%
#     mutate(
#       year = year(date_object),
#       DOY = yday(date_object),
#       is_night = .data[[par_var]] < night_threshold
#     )
#   
#   unique_sites <- unique(daily_data$site)
#   results_list <- list()
#   issues_list <- list()
#   
#   for (site_id in unique_sites) {
#     site_data <- daily_data %>% filter(site == site_id)
#     site_lat <- site_data$LOCATION_LAT[1]
#     unique_years <- sort(unique(site_data$year))
#     
#     for (yr in unique_years) {
#       yr_data <- site_data %>% filter(year == yr)
#       if (nrow(yr_data) < 100) next
#       
#       # 5-day moving window
#       window_results <- list()
#       window_days <- sort(unique(yr_data$DOY))
#       
#       for (start_day in window_days) {
#         days <- seq(start_day, start_day + 4)
#         win_data <- yr_data %>% filter(DOY %in% days)
#         
#         if (nrow(win_data) < 20) next
#         
#         # Estimate nighttime Reco as mean NEE when PAR is low
#         reco <- win_data %>% filter(is_night) %>% pull(.data[[nee_var]]) %>% mean(na.rm = TRUE)
#         if (is.na(reco)) next
#         
#         # Estimate GPP
#         win_data <- win_data %>%
#           mutate(GPP_est = reco - .data[[nee_var]]) %>%
#           filter(!is.na(GPP_est), GPP_est > 0, .data[[par_var]] > 0)
#         
#         if (nrow(win_data) < 10) next
#         
#         # Fit light response curve: GPP = (a * PAR)/(1 + b * PAR), with b fixed
#         b_fixed <- 0.002
#         tryCatch({
#           model <- nlsLM(GPP_est ~ (a * .data[[par_var]]) / (1 + b_fixed * .data[[par_var]]),
#                          data = win_data,
#                          start = list(a = max(win_data$GPP_est, na.rm = TRUE)),
#                          control = nls.lm.control(maxiter = 100))
#           a_fit <- coef(model)[["a"]]
#           gpp1800 <- (a_fit * 1800) / (1 + b_fixed * 1800)
#           
#           midpoint_day <- median(win_data$DOY)
#           window_results[[length(window_results) + 1]] <- tibble(
#             site = site_id,
#             year = yr,
#             DOY = midpoint_day,
#             GPP1800 = gpp1800
#           )
#         }, error = function(e) {})
#       }
#       
#       # Compile and fit logistic curves to GPP1800 time series
#       gpp_ts <- bind_rows(window_results)
#       if (nrow(gpp_ts) < 30) {
#         issues_list[[length(issues_list) + 1]] <- tibble(site = site_id, year = yr, reason = "Insufficient data for logistic fit")
#         next
#       }
#       
#       tryCatch({
#         gpp_ts <- gpp_ts %>% arrange(DOY)
#         
#         # Spring logistic fit
#         spring <- gpp_ts %>% filter(DOY <= 180)
#         fall   <- gpp_ts %>% filter(DOY > 180)
#         
#         fit_logistic <- function(df) {
#           nlsLM(GPP1800 ~ A / (1 + exp(-(DOY - x0)/k)),
#                 data = df,
#                 start = list(A = max(df$GPP1800), x0 = median(df$DOY), k = 10),
#                 control = nls.lm.control(maxiter = 500))
#         }
#         
#         spring_fit <- fit_logistic(spring)
#         fall_fit <- fit_logistic(fall)
#         
#         predict_logistic <- function(fit, days) {
#           A <- coef(fit)[["A"]]
#           x0 <- coef(fit)[["x0"]]
#           k <- coef(fit)[["k"]]
#           A / (1 + exp(-(days - x0)/k))
#         }
#         
#         doy_seq <- 1:365
#         gpp_spring <- predict_logistic(spring_fit, doy_seq)
#         gpp_fall <- predict_logistic(fall_fit, doy_seq)
#         
#         # Merge spring and fall
#         gpp_all <- tibble(DOY = doy_seq,
#                           GPP1800 = ifelse(DOY <= 180, gpp_spring, gpp_fall))
#         
#         max_gpp <- max(gpp_all$GPP1800, na.rm = TRUE)
#         threshold <- 0.25 * max_gpp
#         
#         SOS <- gpp_all %>% filter(DOY <= 180, GPP1800 >= threshold) %>% slice_head(n = 1)
#         EOS <- gpp_all %>% filter(DOY > 180, GPP1800 <= threshold) %>% slice_head(n = 1)
#         
#         results_list[[length(results_list) + 1]] <- tibble(
#           site = site_id,
#           year = yr,
#           SOS_light = SOS$DOY,
#           EOS_light = EOS$DOY
#         )
#       }, error = function(e) {
#         issues_list[[length(issues_list) + 1]] <- tibble(site = site_id, year = yr, reason = "Logistic fit failed")
#       })
#     }
#   }
#   
#   return(list(
#     phenology_df = bind_rows(results_list),
#     issues_df = bind_rows(issues_list)
#   ))
# }
# 
# light_response_result <- detect_phenology_light_response(daily_data)

#daily_data <- load_and_clean_daily_data()
plot_seasonal_cycle(daily_data, flux_var = "GPP_NT_VUT_REF", y_mode = "full")

# There appear to be duplicates, so let's get rid of those
daily_data2 <- daily_data %>% distinct(site, date_object, .keep_all = TRUE)
nrow(daily_data)
nrow(daily_data2)

# # Run the phenology detection on your daily data

phen_results <- detect_phenology_integral(
  daily = daily_data2,
  date_var = "date_object",
  knots = 10,
  flux_var = "GPP_NT_VUT_REF"
)
# daily_data$NE


# 
# # The result is a list with two data frames:
# phenology_df <- phen_results$phenology_df
# issues_df    <- phen_results$issues_df
# 
# illustrate_integral_smoothing_example(daily_data = daily_data)
# # View successful phenology estimates
phen_results %>% filter(is.na(issues))
# 
# # View records where phenology estimation failed or was suspicious
phen_results %>% filter(!is.na(issues))
# 
# # Optional: filter and inspect just order violations
phen_results %>% filter(issues == "Order violation (SOS >= POS or POS >= EOS)")
# # Plot the time series for SOS, POS, EOS grouped by IGBP type
phen_plots <- plot_phenology_timeseries(phen_results, site_metadata)


# Example: Show the POS timing for Forest group
# print(phen_plots$SOS_Forest) #Not sure what this was supposed to show, but its not something produced by plot_phenology_timeseries


plot_flux_timeseries_by_igbp(annual_data, flux_var = "LE_F_MDS")
plot_flux_timeseries_by_igbp(annual_data, flux_var = "NEE_VUT_REF")
plot_flux_timeseries_by_igbp(annual_data, flux_var = "RECO_NT_VUT_REF")
plot_flux_timeseries_by_igbp(annual_data, flux_var = "GPP_NT_VUT_REF")
plot_flux_timeseries_by_igbp(annual_data, flux_var = "WUE")



