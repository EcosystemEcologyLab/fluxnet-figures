# scripts/plotting_functions.R

# Load necessary libraries
library(ggplot2)

# Function to plot comparison results
plot_comparison <- function(comparison_table, group_name, group_value) {
  p <- ggplot(comparison_table, aes(x = standardized_gez_name, y = Difference)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(
      title = paste("Difference between IPCC Emission Factors and FLUXNET NEE in", group_value),
      x = "FAO Global Ecological Zones",
      y = "Difference (MgC Ha-1 yr-1)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0.5)
    )
  return(p)
}

# Function to plot the boxplot
plot_emission_factor_boxplot <- function(fluxnet_data, IPCC_EF) {
  p <- ggplot(fluxnet_data, aes(x = standardized_gez_name, y = NEE_transformed)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "white", colour = "black") +
    geom_point(data = IPCC_EF, aes(x = standardized_gez_name, y = IPCC_MgC_ha_y), color = "cyan") +
    labs(title = "Emission Factor Distribution by FAO Global Ecological Zones", y = "NEP (MgC Ha-1 yr-1)") +
    theme_minimal() +
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, size = 16),
          axis.text.y = element_text(size = 18),
          legend.position = "none") +
    scale_y_continuous(limits = c(-10, 8), breaks = seq(-10, 8, by = 2))
  return(p)
}

# Function to plot the difference
plot_difference <- function(merged_data) {
  p <- ggplot(merged_data, aes(x = standardized_gez_name, y = Difference)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Difference between IPCC Emission Factors and NEE Transformed",
         x = "FAO Global Ecological Zones",
         y = "Difference (MgC Ha-1 yr-1)") +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12))
  return(p)
}

# Function to plot mean values with 95% CI
plot_EF_Bias_values <- function(merged_data) {
  p <- ggplot(merged_data, aes(x = standardized_gez_name)) +
    geom_point(aes(y = mean_NEE_transformed, color = "FLUXNET"), size = 3) +
    geom_errorbar(aes(ymin = lower_CI_NEE, ymax = upper_CI_NEE, color = "FLUXNET"), width = 0.2) +
    geom_point(aes(y = mean_IPCC_MgC_ha_y, color = "IPCC"), size = 3) +
    geom_errorbar(aes(ymin = lower_CI_IPCC, ymax = upper_CI_IPCC, color = "IPCC"), width = 0.2) +
    scale_color_manual(values = c("FLUXNET" = "black", "IPCC" = "cyan")) +
    labs(title = "Mean Values and 95% Confidence Intervals",
         x = "FAO Global Ecological Zones",
         y = "Emissions Factors (MgC Ha-1 yr-1)",
         color = "Data Source") +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    coord_cartesian(ylim = c(-2, 7.5))
  return(p)
}


# Define consistent theme functions
consistent_theme_vertical <- function() {
  theme_minimal() +
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
}

consistent_theme_horizontal <- function() {
  theme_minimal() +
    theme(
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.text.y = element_text(angle = 0, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.position = "top",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
}

# Function to plot boxplot for a given grouping variable
plot_diagnostic_boxplot <- function(data, group_var, y_var, y_label = NULL) {
  # Use default y-axis label if not provided
  if (is.null(y_label)) {
    y_label <- y_var
  }
  
  ggplot(data, aes_string(x = group_var, y = y_var)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, fill = "lightgray", colour = "black") +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
    labs(
      title = paste("Distribution of", y_var, "by", str_to_title(str_replace_all(group_var, "_", " "))),
      y = y_label,
      x = str_to_title(str_replace_all(group_var, "_", " "))
    ) +
    consistent_theme_vertical()
}
