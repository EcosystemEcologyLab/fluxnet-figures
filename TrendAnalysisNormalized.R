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


# Source utility and plotting scripts
source("R/fcn_utility_FLUXNET.R")
source("R/fcn_plot_FLUXNET.R")

# Load metadata
site_metadata <- load_fluxnet_metadata()

# Load and clean data
annual_data <- load_and_clean_annual_data(site_metadata)

annual_data <- annual_data%>%
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
