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



# Source utility and plotting scripts
source("R/fcn_utility_FLUXNET.R")
source("R/fcn_plot_FLUXNET.R")

# Load metadata
site_metadata <- load_fluxnet_metadata()

# Load and clean data
annual_data <- load_and_clean_annual_data(site_metadata)
#daily_data  <- load_and_clean_daily_data(site_metadata)

# ------------------------
# 1. Prepare GPP Data
# ------------------------
# Function to create site-level annual GPP time series and anomalies
prepare_gpp_anomalies <- function(annual_data, gpp_var = "GPP_NT_VUT_REF") {
  annual_data %>%
    filter(!is.na(.data[[gpp_var]])) %>%
    mutate(year = as.numeric(TIMESTAMP)) %>%
    group_by(site) %>%
    # Compute anomalies for each site
    mutate(
      site_mean_gpp = mean(.data[[gpp_var]], na.rm = TRUE),
      gpp_anomaly = .data[[gpp_var]] - site_mean_gpp
    ) %>%
    ungroup()
}

# ------------------------
# 2. Network-Wide GPP Anomaly Timeseries
# ------------------------
# Function to average anomalies across all sites for each year
aggregate_gpp_anomalies <- function(gpp_anomalies) {
  gpp_anomalies %>%
    group_by(year) %>%
    summarize(
      mean_anomaly = mean(gpp_anomaly, na.rm = TRUE),
      n_sites = n_distinct(site),
      .groups = "drop"
    )
}

# ------------------------
# 3. Trend Estimation
# ------------------------
# Fit linear trend and Mann-Kendall test
calculate_gpp_trend <- function(network_ts) {
  # Mann-Kendall test
  mk_test <- trend::mk.test(network_ts$mean_anomaly)
  
  # Theil-Sen slope
  sens <- trend::sens.slope(network_ts$mean_anomaly)
  
  list(
    slope = sens$estimates,       # Theil-Sen slope
    intercept = NA,               # Not estimated directly
    mk_tau = mk_test$estimates[["tau"]],
    mk_pval = mk_test$p.value
  )
}

# ------------------------
# 4. Bootstrapped Uncertainty
# ------------------------
# Resample site-years to get interquartile range of GPP trends
bootstrap_gpp_trends <- function(gpp_anomalies, n_boot = 5000) {
  boot_slopes <- replicate(n_boot, {
    boot_data <- gpp_anomalies %>%
      group_by(site) %>%
      sample_frac(1, replace = TRUE) %>%
      ungroup() %>%
      group_by(year) %>%
      summarize(mean_anomaly = mean(gpp_anomaly, na.rm = TRUE), .groups = "drop")
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
# 5. Visualization
# ------------------------
plot_gpp_anomaly_trend <- function(network_ts, trend_results) {
  ggplot(network_ts, aes(x = year, y = mean_anomaly)) +
    geom_point(size = 2, color = "black") +
    geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 1) +
    labs(
      title = paste0("Network-wide GPP Anomalies (Trend = ",
                     round(trend_results$slope, 2), " gC m⁻² yr⁻²)"),
      x = "Year",
      y = expression(paste("GPP Anomaly (gC m"^{-2}, " yr"^{-1}, ")"))
    ) +
    theme_classic(base_size = 14)
}

# ------------------------
# 6. Run Analysis
# ------------------------
run_gpp_trend_analysis <- function(annual_data, gpp_var = "GPP_NT_VUT_REF") {
  gpp_anomalies <- prepare_gpp_anomalies(annual_data, gpp_var)
  network_ts <- aggregate_gpp_anomalies(gpp_anomalies)
  trend_results <- calculate_gpp_trend(network_ts)
  bootstrap_results <- bootstrap_gpp_trends(gpp_anomalies)
  plot_obj <- plot_gpp_anomaly_trend(network_ts, trend_results)
  
  list(
    gpp_anomalies = gpp_anomalies,
    network_ts = network_ts,
    trend_results = trend_results,
    bootstrap_results = bootstrap_results,
    plot = plot_obj
  )
}

# ------------------------
# Example Usage
# ------------------------
# Assuming annual_data is loaded from fluxnet_workflow_refactor.R:
# annual_data <- load_and_clean_annual_data()

# Run GPP Trend Analysis
gpp_trend_output <- run_gpp_trend_analysis(annual_data)

# Print results
print(gpp_trend_output$trend_results)
print(gpp_trend_output$bootstrap_results)

# Plot GPP anomalies
print(gpp_trend_output$plot)
