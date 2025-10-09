# fluxnet_gpp_trend_analysis.R
# Observational GPP Trend Analysis (Chen et al. 2022)
# Authors: Dave Moore & ChatGPT

# ------------------------
# Load Libraries
# ------------------------
library(dplyr)
library(ggplot2)
library(zyp)     # Mann-Kendall trend test
library(purrr)
library(trend)
library(countrycode)
library(tidyr)
library(forcats)


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


# Buckets we care about (per your preference: ONLY FLUXNET2015 FULLSET + ICOS L2)
mani_FULL <- manifest %>%
  dplyr::filter(data_center == "FLX",
                data_product == "FLUXNET2015",
                time_integral == "YY",
                dataset == "FULLSET")

mani_L2 <- manifest %>%
  dplyr::filter(data_center == "ICOSETC",
                data_product == "FLUXNET",
                time_integral == "YY",
                dataset == "L2")

# Load buckets fresh (no cache), clean sentinels, attach 'year'
annual_full <- load_fluxnet_data(manifest = mani_FULL, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  { . ->> .annual_full_raw } %>%    # keep a raw copy if you want to inspect later
  dplyr::mutate(year = year_from_df(.)) %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

annual_l2 <- load_fluxnet_data(manifest = mani_L2, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  { . ->> .annual_l2_raw } %>%
  dplyr::mutate(year = year_from_df(.)) %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

# Per-site year ranges in each bucket
ranges_full <- annual_full %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(flx_min = min(year), flx_max = max(year), .groups = "drop")

ranges_l2 <- annual_l2 %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(l2_min = min(year), l2_max = max(year), .groups = "drop")

# Bridge / handoff table (who ends in FULLSET and where L2 resumes)
bridge <- ranges_full %>%
  dplyr::left_join(ranges_l2, by = "site") %>%
  dplyr::mutate(
    cliff_2014 = flx_max == 2014,
    has_L2     = !is.na(l2_min),
    gap_years  = dplyr::if_else(has_L2, pmax(0L, l2_min - flx_max - 1L), NA_integer_)
  )

# Clip to avoid overlap and then combine
annual_full_clip <- annual_full %>%
  dplyr::inner_join(bridge %>% dplyr::select(site, flx_max), by = "site") %>%
  dplyr::filter(!is.na(year) & year <= flx_max) %>%
  dplyr::mutate(source_bucket = "FULLSET")

annual_l2_clip <- annual_l2 %>%
  dplyr::inner_join(bridge %>% dplyr::select(site, l2_min), by = "site") %>%
  dplyr::filter(!is.na(year) & year >= l2_min) %>%
  dplyr::mutate(source_bucket = "L2")

# Combined ICOS annual series (prefer L2 if any same-year duplicates appear)
annual_icos_combined <- dplyr::bind_rows(annual_full_clip, annual_l2_clip) %>%
  dplyr::arrange(site, year, dplyr::desc(source_bucket)) %>%  # L2 > FULLSET on ties
  dplyr::group_by(site, year) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# (Optional) Quick coverage sanity tables
cov_full <- annual_full_clip %>% dplyr::count(year, name = "n_site_years") %>% dplyr::mutate(bucket = "FULLSET")
cov_l2   <- annual_l2_clip   %>% dplyr::count(year, name = "n_site_years") %>% dplyr::mutate(bucket = "L2")
cov_comb <- annual_icos_combined %>% dplyr::count(year, name = "n_site_years") %>% dplyr::mutate(bucket = "COMBINED")
coverage_by_year <- dplyr::bind_rows(cov_full, cov_l2, cov_comb) %>% dplyr::arrange(year, bucket)

# Your “annual” object can now be:
annual <- annual_icos_combined


# -----------------------------------------------------------------------------
# 3) Load annual (YY) FULLSET data using the manifest
#    - Replaces -9999 sentinels with NA
#    - Adds integer year column
#    - Joins site metadata (IGBP, LAT/LON, etc.) to the annual records
# -----------------------------------------------------------------------------
annual <- manifest %>%
  filter(time_integral == "YY", dataset %in% c("FULLSET","SUBSET", "L2")) %>%
  load_fluxnet_data() %>%                                    # reads 383 files here
  mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%# sentinel → NA
  mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
  left_join(metadata %>% select(-SITEID, -SITE_ID), by = join_by(site)) 



# -----------------------------------------------------------------------------
# 4) Gate by "percent gap-filled" at the annual scale
#    - We use the *_QC that accompanies your gating variable at YY.
#    - For NEE: NEE_VUT_REF_QC in [0..1]. We interpret (1 - QC) as % gap-filled.
#    - Default rule: "bad" if > 50% gap-filled (you can change this easily).
# -----------------------------------------------------------------------------

# ---- user-tunable knobs ----
qc_gate_var         <- c("NEE_VUT_REF", "PA_F")  # e.g., "NEE_VUT_REF", "GPP_NT_VUT_REF", "LE_F", etc.
max_gapfilled_bad   <- 0.50           # >50% filled => bad
drop_if_qc_missing  <- TRUE           # if QC missing at YY, drop the record

# change the rule: set max_gapfilled_bad <- 0.25 (require ≥75% “good”).
# Gate on GPP instead of NEE: qc_gate_var <- "GPP_NT_VUT_REF".
# Keep records with missing QC: set drop_if_qc_missing <- FALSE.


# helper that adds pct_gapfilled + is_bad, based on the YY QC column for the chosen var
annual_flagged <- annual |>
  flag_bad_gapfilled(
    gate_vars        = qc_gate_var,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

# keep the good stuff; also keep a small audit table of exclusions
annual_clean <- annual_flagged |>
  dplyr::filter(!is_bad)

annual_data <- annual_clean%>%
  mutate(
    Alpha2 = substr(site, 1, 2),
    Continent = countrycode(Alpha2, origin = 'iso2c', destination = 'continent')
  )
#daily_data  <- load_and_clean_daily_data(site_metadata)


# ------------------------
# 1. Prepare Flux Anomalies
# ------------------------
# For each site:
#   Compute the mean value of the flux across all available years.
# Subtract this mean from each year’s value to calculate the anomaly.
# Save this difference in a new column flux_anomaly.

prepare_flux_anomalies <- function(data, flux_var) {
  data %>%
    filter(!is.na(.data[[flux_var]])) %>%
    mutate(year = as.numeric(TIMESTAMP)) %>%
    group_by(site) %>%
    mutate(
      site_mean_flux = mean(.data[[flux_var]], na.rm = TRUE),
      flux_anomaly = .data[[flux_var]] - site_mean_flux
    ) %>%
    ungroup()
}

# ------------------------
# 2. Network-Wide Aggregation
# ------------------------
# For each year:
# Compute the mean of the anomalies across all available sites.
# Count how many unique sites contributed data that year (n_sites).

aggregate_flux_anomalies <- function(anomalies) {
  anomalies %>%
    group_by(year) %>%
    summarize(
      mean_anomaly = mean(flux_anomaly, na.rm = TRUE),
      n_sites = n_distinct(site),
      .groups = "drop"
# .groups = "drop" tells dplyr not to retain the group structure in the output.
    )
}

# ------------------------
# 3. Trend Estimation
# ------------------------
calculate_flux_trend <- function(network_ts) {
  mk_test <- trend::mk.test(network_ts$mean_anomaly)
  sens <- trend::sens.slope(network_ts$mean_anomaly)
  list(
    slope = sens$estimates,
    intercept = NA,
    mk_tau = mk_test$estimates[["tau"]],
    mk_pval = mk_test$p.value
  )
}

# ------------------------
# 4. Bootstrapping
# ------------------------
bootstrap_flux_trends <- function(anomalies, n_boot = 5000) {
  boot_slopes <- replicate(n_boot, {
    boot_data <- anomalies %>%
      group_by(site) %>%
      sample_frac(1, replace = TRUE) %>%
      ungroup() %>%
      group_by(year) %>%
      summarize(mean_anomaly = mean(flux_anomaly, na.rm = TRUE), .groups = "drop")
    coef(lm(mean_anomaly ~ year, data = boot_data))[["year"]]
  })
  
  tibble(
    slope_median = median(boot_slopes),
    slope_IQR = IQR(boot_slopes),
    slope_lower = quantile(boot_slopes, 0.25),
    slope_upper = quantile(boot_slopes, 0.75)
  )
}

# ------------------------
# 5. Network-Wide Trend Plot
# ------------------------
plot_network_flux_trend <- function(network_ts, trend_results, flux_var) {
  ggplot(network_ts, aes(x = year, y = mean_anomaly)) +
    geom_point(size = 2, color = "black") +
    geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 1) +
    labs(
      title = paste0("Network-wide ", flux_var, " Anomalies (Trend = ",
                     round(trend_results$slope, 2), ")"),
      x = "Year",
      y = paste0(flux_var, " Anomaly")
    ) +
    theme_classic(base_size = 14)
}

# ------------------------
# 6. Site-Level Grouped Anomaly Plot
# ------------------------
plot_site_anomalies_by_group <- function(anomalies, group_var = "IGBP", color_var = "Continent") {
  ggplot(anomalies, aes(x = year, y = flux_anomaly, group = site)) +
    geom_line(alpha = 0.4) +
    facet_wrap(as.formula(paste("~", group_var))) +
    aes_string(color = color_var) +
    theme_classic(base_size = 12) +
    labs(y = "Flux Anomaly", x = "Year", color = color_var)
}

# ------------------------
# 7. Diagnostics
# ------------------------
diagnostic_plots <- function(anomalies, flux_var) {
  p1 <- anomalies %>%
    count(site) %>%
    ggplot(aes(x = n)) +
    geom_histogram(binwidth = 1, fill = "grey", color = "black") +
    theme_minimal() +
    labs(title = "Years of Data per Site", x = "# Years", y = "# Sites")
  
  p2 <- ggplot(anomalies, aes(x = flux_anomaly)) +
    geom_density(fill = "steelblue", alpha = 0.6) +
    theme_minimal() +
    labs(title = paste0("Distribution of ", flux_var, " Anomalies"))
  
  list(hist_years = p1, density_anomaly = p2)
}

# ------------------------
# 8. Master Function
# ------------------------
run_flux_trend_analysis <- function(data, flux_var = "GPP_NT_VUT_REF", group_var = "IGBP", color_var = "Continent") {
  anomalies <- prepare_flux_anomalies(data, flux_var)
  network_ts <- aggregate_flux_anomalies(anomalies)
  trend_results <- calculate_flux_trend(network_ts)
  bootstrap_results <- bootstrap_flux_trends(anomalies)
  plot_trend <- plot_network_flux_trend(network_ts, trend_results, flux_var)
  plot_grouped <- plot_site_anomalies_by_group(anomalies, group_var, color_var)
  diagnostics <- diagnostic_plots(anomalies, flux_var)
  
  list(
    anomalies = anomalies,
    network_ts = network_ts,
    trend_results = trend_results,
    bootstrap_results = bootstrap_results,
    plot_trend = plot_trend,
    plot_grouped = plot_grouped,
    diagnostics = diagnostics
  )
}


# ------------------------
# 9. Grouped Trend Analysis (e.g., by Continent or IGBP)
# ------------------------
run_flux_trend_by_group <- function(data, flux_var = "GPP_NT_VUT_REF", group_var = "Continent") {
  anomalies <- prepare_flux_anomalies(data, flux_var)
  
  grouped_anomalies <- anomalies %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]], year) %>%
    summarize(
      mean_anomaly = mean(flux_anomaly, na.rm = TRUE),
      n_sites = n_distinct(site),
      .groups = "drop"
    ) %>%
    rename(group = 1)  # rename to 'group' for downstream consistency
  
  # Filter out groups with < 3 years of data
  valid_groups <- grouped_anomalies %>%
    group_by(group) %>%
    filter(n() >= 3) %>%
    pull(group) %>%
    unique()
  
  grouped_filtered <- grouped_anomalies %>%
    filter(group %in% valid_groups)
  
  # Trend estimation per valid group
  trend_results <- grouped_filtered %>%
    group_by(group) %>%
    group_map(~{
      mk <- trend::mk.test(.x$mean_anomaly)
      sens <- trend::sens.slope(.x$mean_anomaly)
      tibble(
        group = unique(.x$group),
        slope = sens$estimates,
        mk_tau = mk$estimates[["tau"]],
        mk_pval = mk$p.value
      )
    }) %>%
    bind_rows()
  
  skipped_groups <- setdiff(unique(grouped_anomalies$group), valid_groups)
  if (length(skipped_groups) > 0) {
    warning("Skipped groups with fewer than 3 years of data: ", paste(skipped_groups, collapse = ", "))
  }  
  # Plot results
  trend_plot <- ggplot(grouped_filtered, aes(x = year, y = mean_anomaly)) +
    geom_point(size = 1, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    facet_wrap(~ group, scales = "free_y") +
    theme_classic(base_size = 13) +
    labs(
      title = paste("Trends in", flux_var, "Anomalies by", group_var),
      x = "Year",
      y = paste(flux_var, "Anomaly")
    )
  
  list(
    grouped_anomalies = grouped_filtered,
    trend_results = trend_results,
    plot = trend_plot
  )
}


# Trend analysis by Continent
by_continent <- run_flux_trend_by_group(annual_data, flux_var = "NEE_VUT_REF", group_var = "Continent")
print(by_continent$trend_results)
print(by_continent$plot)

# Trend analysis by IGBP
by_igbp <- run_flux_trend_by_group(annual_data, flux_var = "NEE_VUT_REF", group_var = "IGBP")
print(by_igbp$trend_results)
print(by_igbp$plot)

result <- run_flux_trend_analysis(
  data = annual_data,
  flux_var = "NEE_VUT_REF",
  group_var = "Continent",
  color_var = "IGBP"
)

annual_data$gpp

print(result$trend_results)
print(result$bootstrap_results)
print(result$plot_trend)
print(result$plot_grouped)
print(result$diagnostics$hist_years)
print(result$diagnostics$density_anomaly)
