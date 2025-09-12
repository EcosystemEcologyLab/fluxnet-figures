# demo_fluxnet_plots.R (streamlined)
# ----------------------------------
# Requires: R/fcn_utility_FLUXNET.R, R/fcn_plot_FLUXNET.R
# Preserves the same figures; reduces duplication and adds helpers.

# ---- Sources ----
source("R/fcn_utility_FLUXNET.R")
source("R/fcn_plot_FLUXNET.R")

# ---- Libraries (explicit namespaces to avoid masking) ----
library(dplyr)
library(ggplot2)
library(rlang)
library(raster)   # per your preference (not terra)
library(ggnewscale)
library(plotbiomes)

# --- Ensure dplyr verbs are used (avoid masking by other packages) ---
select     <- dplyr::select
filter     <- dplyr::filter
mutate     <- dplyr::mutate
rename     <- dplyr::rename
arrange    <- dplyr::arrange
summarise  <- dplyr::summarise
distinct   <- dplyr::distinct
left_join  <- dplyr::left_join
bind_rows  <- dplyr::bind_rows
semi_join  <- dplyr::semi_join
join_by    <- dplyr::join_by


# -------------------------------
# Config: paths, palettes, flags
# -------------------------------
cfg <- list(
  amf_dir     = here::here("data/FLUXNET/AMF"),
  icos_dir    = here::here("data/FLUXNET/ICOS"),
  ai_tif      = "~/Documents/RProjects/data/AI_4km.tif",
  wc_rds      = "data/wc_worldclim_30s.rds",
  ai_limit    = 0.65,
  out_dir     = "saved_plots",
  aridity_cols = c(
    "Hyper-Arid"    = "red",
    "Arid"          = "orange",
    "Semi-Arid"     = "yellow",
    "Dry Sub-Humid" = "lightgreen",
    "Humid"         = "darkgreen"
  )
)

if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE)

# --------------------------------
# Tiny helpers for reuse/clarity
# --------------------------------
theme_fluxnet <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.background = element_rect(color = "black", fill = NA),
      legend.position = "bottom"
    )
}

flux_label <- function(var) {
  # use expression() per your preference
  if (startsWith(var, "NEE"))  return(expression(NEE~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "GPP"))  return(expression(GPP~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "RECO")) return(expression(Reco~(mu*mol~m^{-2}~s^{-1})))
  if (var == "LE_F_MDS")       return(expression(LE~(W~m^{-2})))
  if (var == "WUE")            return(expression(WUE==GPP/LE))
  as.name(var)
}

guess_latlon_cols <- function(metadata) {
  lat_candidates <- c("LOCATION_LAT","LATITUDE","latitude","lat","SITE_LAT")
  lon_candidates <- c("LOCATION_LONG","LONGITUDE","longitude","lon","long","SITE_LON")
  lat_col <- intersect(lat_candidates, names(metadata))[1]
  lon_col <- intersect(lon_candidates, names(metadata))[1]
  
  if (is.na(lat_col) || is.na(lon_col)) {
    numdf <- dplyr::select(metadata, where(is.numeric))
    stopifnot(ncol(numdf) > 0)
    lat_col <- names(sort(vapply(numdf, function(x) mean(x >= -90 & x <= 90, na.rm = TRUE), 0), decreasing = TRUE))[1]
    lon_col <- names(sort(vapply(numdf, function(x) mean(x >= -180 & x <= 180, na.rm = TRUE), 0), decreasing = TRUE))[1]
  }
  list(lat = lat_col, lon = lon_col)
}

attach_aridity <- function(annual, metadata, ai_tif, ai_limit = NULL) {
  stopifnot(file.exists(ai_tif))
  cols <- guess_latlon_cols(metadata)
  
  meta_coords <- metadata %>%
    dplyr::rename(latitude = !!sym(cols$lat), longitude = !!sym(cols$lon)) %>%
    dplyr::select(site, latitude, longitude) %>%
    dplyr::mutate(
      latitude  = suppressWarnings(as.numeric(latitude)),
      longitude = suppressWarnings(as.numeric(longitude)),
      longitude = ifelse(longitude > 180, longitude - 360, longitude)
    ) %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
    dplyr::semi_join(dplyr::distinct(annual, site), by = "site")
  
  stopifnot(nrow(meta_coords) > 0)
  
  ai_r <- raster::raster(ai_tif)
  if (!raster::isLonLat(ai_r)) {
    ai_r <- raster::projectRaster(ai_r, crs = sp::CRS("+proj=longlat +datum=WGS84"))
  }
  
  meta_ai <- meta_coords %>%
    dplyr::mutate(
      AI = raster::extract(ai_r, cbind(longitude, latitude)),
      Aridity_Class = dplyr::case_when(
        is.na(AI) ~ NA_character_,
        AI < 0.05 ~ "Hyper-Arid",
        AI < 0.20 ~ "Arid",
        AI < 0.50 ~ "Semi-Arid",
        AI < 0.65 ~ "Dry Sub-Humid",
        TRUE      ~ "Humid"
      )
    )
  
  out <- annual %>%
    dplyr::left_join(dplyr::select(meta_ai, site, AI, Aridity_Class), by = "site")
  
  if (!is.null(ai_limit)) out <- dplyr::filter(out, !is.na(AI), AI <= ai_limit)
  out
}

save_plot <- function(p, filename, width = 7, height = 5, dpi = 300) {
  ggsave(file.path(cfg$out_dir, filename), p, width = width, height = height, dpi = dpi)
}

# -------------------------
# 1) Metadata + manifest
# -------------------------
metadata <- load_fluxnet_metadata()

amf_files  <- discover_AMF_files(data_dir = cfg$amf_dir)
icos_files <- discover_ICOS_files(data_dir = cfg$icos_dir)

manifest <- dplyr::bind_rows(amf_files, icos_files) %>%
  dplyr::distinct(
    site, data_product, dataset, time_integral, start_year, end_year,
    .keep_all = TRUE
  )

# -------------------------
# 2) Load annual data
# -------------------------
annual <- manifest %>%
  dplyr::filter(time_integral == "YY", dataset == "FULLSET") %>%
  load_fluxnet_data() %>%
  dplyr::mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%
  dplyr::mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID), by = dplyr::join_by(site))

# -----------------------------------------
# 3) Attach Aridity Index (only once!)
# -----------------------------------------
annual_ai <- attach_aridity(annual, metadata, cfg$ai_tif, ai_limit = cfg$ai_limit)

annual_ai_all <- attach_aridity(annual, metadata, ai_tif = "~/Documents/RProjects/data/AI_4km.tif", ai_limit = NULL)


# (optional sanity)
# print(table(annual_ai$Aridity_Class, useNA = "ifany"))
# summary(annual_ai$AI)

# -----------------------------------------
# 4) Quick aridity-aware plot helpers
# -----------------------------------------
plot_flux_vs_ai <- function(annual_ai_data, flux_var = "NEE_VUT_REF", point_alpha = 0.35) {
  df <- annual_ai_data %>%
    dplyr::filter(!is.na(AI)) %>%
    dplyr::mutate(
      FLUX = if (flux_var == "WUE") GPP_NT_VUT_REF / LE_F_MDS else .data[[flux_var]]
    )
  
  ggplot(df, aes(x = AI, y = FLUX)) +
    geom_point(shape = 21, fill = "black", color = "black", size = 2, alpha = point_alpha, stroke = 0.2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8, alpha = 0.15) +
    labs(x = "Aridity Index (AI)", y = flux_label(flux_var),
         title = paste0(flux_var, " vs Aridity Index")) +
    theme_fluxnet()
}

plot_xy_by_aridity <- function(annual_ai_data, x_var = "GPP_NT_VUT_REF", y_var = "RECO_NT_VUT_REF",
                               point_alpha = 0.6, aridity_cols = cfg$aridity_cols) {
  df <- annual_ai_data %>%
    dplyr::filter(!is.na(AI)) %>%
    dplyr::mutate(
      X = if (x_var == "WUE") GPP_NT_VUT_REF / LE_F_MDS else .data[[x_var]],
      Y = if (y_var == "WUE") GPP_NT_VUT_REF / LE_F_MDS else .data[[y_var]],
      Aridity_Class = factor(Aridity_Class,
                             levels = c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid","Humid"))
    )
  
  ggplot(df, aes(x = X, y = Y, fill = Aridity_Class)) +
    geom_point(shape = 21, color = "black", size = 2.2, alpha = point_alpha, stroke = 0.25) +
    scale_fill_manual(values = aridity_cols, name = "Aridity Class") +
    labs(x = flux_label(x_var), y = flux_label(y_var),
         title = paste0(y_var, " vs ", x_var, " by Aridity Class")) +
    theme_fluxnet()
}

# ---------------------------------------------------
# 5) Generate the same figures you currently create
#     (compact loops; identical content)
# ---------------------------------------------------
message("Generating Flux vs AI…")
plots_flux_vs_ai <- setNames(
  lapply(c("NEE_VUT_REF","GPP_NT_VUT_REF"), \(v) plot_flux_vs_ai(annual_ai_all, flux_var = v)),
  c("NEE_vs_AI","GPP_vs_AI")
)
print(plots_flux_vs_ai$NEE_vs_AI);  save_plot(plots_flux_vs_ai$NEE_vs_AI, "ai_nee.png")
print(plots_flux_vs_ai$GPP_vs_AI);  save_plot(plots_flux_vs_ai$GPP_vs_AI, "ai_gpp.png")

message("Generating Flux–Flux by aridity…")
p_xy_gpp_reco <- plot_xy_by_aridity(annual_ai_all, "GPP_NT_VUT_REF", "RECO_NT_VUT_REF")
print(p_xy_gpp_reco); save_plot(p_xy_gpp_reco, "xy_gpp_reco_by_aridity.png")

p_xy_gpp_LE <- plot_xy_by_aridity(annual_ai_all, "LE_F_MDS", "GPP_NT_VUT_REF") +
  ggplot2::coord_cartesian(ylim = c(0, NA))  # y starts at 0; top auto
print(p_xy_gpp_LE)
save_plot(p_xy_gpp_LE, "xy_gpp_LE_by_aridity.png")


p_xy_gpp_LE <- plot_xy_by_aridity(annual_ai, "LE_F_MDS", "GPP_NT_VUT_REF") +
  ggplot2::coord_cartesian(ylim = c(0, NA))  # y starts at 0; top auto
print(p_xy_gpp_LE)
save_plot(p_xy_gpp_LE, "xy_gpp_LE_by_aridity.png")


plot_flux_by_igbp <- function(annual_data, flux_var = "NEE_VUT_REF", y_limits = NULL, title_suffix = "") {
  igbp_order <- c("DBF", "ENF", "MF", "DNF", "EBF", "OSH", "CSH", "WSA", "SAV", "GRA", "CRO", "WET", "BSV")
  
  annual_data <- annual_data %>%
    dplyr::mutate(
      FLUX = .data[[flux_var]],
      flux_sign = factor(ifelse(FLUX < 0, "Negative", "Positive"), levels = c("Negative", "Positive")),
      IGBP = factor(IGBP, levels = igbp_order)
    )
  
  y_label <- if (stringr::str_starts(flux_var, "NEE")) {
    expression(NEE~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "GPP")) {
    expression(GPP~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "RECO")) {
    expression(Reco~(mu*mol~m^{-2}~s^{-1}))
  } else if (stringr::str_starts(flux_var, "LE")) {
    expression(LE~(W~m^{-2}))
  } else if (stringr::str_starts(flux_var, "WUE")) {
    expression(WUE==GPP/LE)
  } else flux_var
  
  p_flux <- ggplot2::ggplot(annual_data, ggplot2::aes(x = IGBP, y = FLUX)) +
    ggplot2::geom_boxplot(color = "black", fill = NA, outlier.shape = NA) +
    ggplot2::geom_jitter(ggplot2::aes(color = flux_sign), width = 0.25, alpha = 0.4, size = 1) +
    ggplot2::scale_color_manual(values = c("Negative" = "#1b9e77", "Positive" = "#d95f02"), name = "Flux Sign") +
    ggplot2::scale_x_discrete(drop = FALSE) +  # keep empty IGBPs so axes match
    ggplot2::labs(x = NULL, y = y_label, title = paste0("Flux by IGBP", title_suffix)) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(panel.background = ggplot2::element_rect(color = "black"),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.position = "bottom") +
    { if (!is.null(y_limits)) ggplot2::coord_cartesian(ylim = y_limits) else ggplot2::coord_cartesian() }
  
  summary_data <- annual_data %>%
    dplyr::group_by(IGBP) %>%
    dplyr::summarize(median_flux = median(FLUX, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(IGBP = factor(IGBP, levels = igbp_order))
  
  p_median <- ggplot2::ggplot(summary_data, ggplot2::aes(x = IGBP, y = median_flux)) +
    ggplot2::geom_col(fill = "black", alpha = 0.6) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = NULL, y = "Median") +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(panel.background = ggplot2::element_rect(color = "black"),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank()) +
    { if (!is.null(y_limits)) ggplot2::coord_cartesian(ylim = y_limits) else ggplot2::coord_cartesian() }
  
  site_counts <- annual_data %>%
    dplyr::group_by(IGBP) %>%
    dplyr::summarize(n_siteyears = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(IGBP = factor(IGBP, levels = igbp_order))
  
  p_count <- ggplot2::ggplot(site_counts, ggplot2::aes(x = IGBP, y = n_siteyears)) +
    ggplot2::geom_col(fill = "gray40", alpha = 0.7) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "IGBP Class", y = "# Site Years") +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(panel.background = ggplot2::element_rect(color = "black"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  p_composite <- p_flux / p_median / p_count + patchwork::plot_layout(heights = c(0.7, 0.2, 0.1))
  
  list(
    flux_plot = p_flux,
    median_plot = p_median,
    count_plot = p_count,
    composite_plot = p_composite
  )
}

# Ensure these exist:
# annual_ai_all  -> your full dataset with columns: site, IGBP, AI, flux vars...
# flux_var       -> choose: "NEE_VUT_REF", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF", etc.

flux_var <- "GPP_NT_VUT_REF"  # change as needed

dry   <- dplyr::filter(annual_ai_all, !is.na(AI) & AI < 0.65)
humid <- dplyr::filter(annual_ai_all, !is.na(AI) & AI > 0.65)

# One common y-range from the full (unfiltered) data:
y_limits <- range(annual_ai_all[[flux_var]], na.rm = TRUE)

# If you want a floor at zero for inherently non-negative variables (e.g., GPP):
# if (stringr::str_starts(flux_var, "GPP") || stringr::str_starts(flux_var, "LE"))
#   y_limits[1] <- 0

plots_dry   <- plot_flux_by_igbp(dry,   flux_var = flux_var, y_limits = y_limits, title_suffix = " (AI < 0.65)")
plots_humid <- plot_flux_by_igbp(humid, flux_var = flux_var, y_limits = y_limits, title_suffix = " (AI > 0.65)")

library(patchwork)
p_side_by_side <- plots_dry$composite_plot | plots_humid$composite_plot
p_side_by_side <- p_side_by_side + patchwork::plot_annotation(
  title = paste0("Flux Distributions by IGBP split at AI = 0.65 (", flux_var, ")")
)

print(p_side_by_side)
save_plot(p_side_by_side, paste0("igbp_composite_split_", flux_var, ".png"), width = 16, height = 8)



# ---------------------------
# House helpers (labels/theme)
# ---------------------------
flux_label <- function(var) {
  if (startsWith(var, "NEE"))  return(expression(NEE~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "GPP"))  return(expression(GPP~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "RECO")) return(expression(Reco~(mu*mol~m^{-2}~s^{-1})))
  if (var == "LE_F_MDS")       return(expression(LE~(W~m^{-2})))
  if (var == "WUE")            return(expression(WUE==GPP/LE))
  as.name(var)
}

theme_bw_inward <- function(base_size = 14) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.position   = "none",
      axis.line.x.top   = ggplot2::element_line(color = "black"),
      axis.line.y.right = ggplot2::element_line(color = "black"),
      axis.ticks.length = grid::unit(-2, "pt"),
      axis.text.x       = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text.y       = ggplot2::element_text(margin = ggplot2::margin(r = 6))
    )
}

# ---------------------------------------------
# 1) Interannual SD and CV% per site (optional detrend)
# ---------------------------------------------
compute_interannual_stats <- function(data,
                                      flux_var = "NEE_VUT_REF",
                                      min_years = 3,
                                      cv_abs_mean = TRUE,
                                      detrend = FALSE,
                                      eps = 1e-8) {
  stopifnot(all(c("site", "year", "AI") %in% names(data)))
  stopifnot(flux_var %in% names(data))
  
  df <- data |>
    dplyr::select(site, year, AI, dplyr::all_of(flux_var)) |>
    dplyr::rename(FLUX = dplyr::all_of(flux_var)) |>
    tidyr::drop_na(FLUX, AI)
  
  # optional detrend per site (linear vs year)
  if (detrend) {
    df <- df |>
      dplyr::group_by(site) |>
      dplyr::mutate(
        .resid = {
          # robust to single-year sites
          if (dplyr::n() >= 2) {
            stats::residuals(stats::lm(FLUX ~ year))
          } else {
            NA_real_
          }
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(FLUX_eff = dplyr::coalesce(.resid, FLUX)) |>
      dplyr::select(-.resid)
  } else {
    df <- dplyr::mutate(df, FLUX_eff = FLUX)
  }
  
  stats_site <- df |>
    dplyr::group_by(site) |>
    dplyr::summarize(
      n_years   = dplyr::n(),
      AI_site   = suppressWarnings(stats::median(AI, na.rm = TRUE)), # constant per site if attached once
      mean_flux = mean(FLUX_eff, na.rm = TRUE),
      sd_flux   = stats::sd(FLUX_eff, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    dplyr::mutate(
      cv_den   = if (cv_abs_mean) abs(mean_flux) else mean_flux,
      cv_pct   = dplyr::if_else(cv_den > eps, 100 * sd_flux / cv_den, NA_real_),
      cv_flag_mean_near_zero = cv_den <= eps
    ) |>
    dplyr::filter(n_years >= min_years)
  
  attr(stats_site, "flux_var") <- flux_var
  attr(stats_site, "detrend")  <- detrend
  stats_site
}

# --------------------------------------------------------
# 2) Plot SD and CV% versus AI (two panels, same x-range)
# --------------------------------------------------------
plot_interannual_variation_vs_ai_colored <- function(
    stats_df,
    flux_var = attr(stats_df, "flux_var"),
    kind = c("sd","cv"),
    smooth = TRUE,
    palette = NULL,
    # --- new controls (mirror the flux plot behavior) ---
    x_limits = NULL,          # visual zoom only (no data dropped from stats)
    y_limits = NULL,          # visual zoom only
    subset_ai = NULL          # OPTIONAL: display-only filter (after stats)
) {
  kind <- match.arg(kind)
  
  # Default palette (override with named vector)
  if (is.null(palette)) {
    palette <- c(
      "Hyper-Arid"    = "red",
      "Arid"          = "orange",
      "Semi-Arid"     = "yellow",
      "Dry Sub-Humid" = "lightgreen",
      "Humid"         = "darkgreen"
    )
  }
  
  # AI → Aridity class (internal so it's always available)
  ai_to_class <- function(ai) dplyr::case_when(
    is.na(ai) ~ NA_character_,
    ai < 0.05 ~ "Hyper-Arid",
    ai < 0.20 ~ "Arid",
    ai < 0.50 ~ "Semi-Arid",
    ai < 0.65 ~ "Dry Sub-Humid",
    TRUE      ~ "Humid"
  )
  
  # Prepare data (no AI truncation here)
  df <- stats_df |>
    dplyr::mutate(
      Aridity_Class = factor(
        ai_to_class(AI_site),
        levels = c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid","Humid")
      )
    ) |>
    dplyr::filter(is.finite(AI_site))  # x must be finite
  
  # Choose y variable + labels
  if (kind == "sd") {
    yvar   <- "sd_flux"
    ylab   <- bquote("Interannual SD of"~.(if (exists("flux_label", mode="function"))
      flux_label(flux_var) else as.name(flux_var)))
    title_ <- paste0("Interannual SD vs Aridity Index (", flux_var, ")")
    y_floor0 <- TRUE
  } else {
    yvar   <- "cv_pct"
    ylab   <- "CV (%)"
    title_ <- paste0("CV% vs Aridity Index (", flux_var, ")")
    y_floor0 <- TRUE
  }
  
  df <- df |> dplyr::filter(is.finite(.data[[yvar]]))
  
  # (Optional) display-only AI subset (after stats are computed)
  if (!is.null(subset_ai)) {
    df <- df |> dplyr::filter(AI_site >= subset_ai[1], AI_site <= subset_ai[2])
  }
  if (!nrow(df)) stop("No finite values to plot after optional subset_ai filter.")
  
  # Subset palette to classes present
  present_lvls <- levels(df$Aridity_Class)
  pal <- palette[present_lvls]; pal[is.na(pal)] <- "grey70"; names(pal) <- present_lvls
  
  # Default axis ranges: show full data unless user supplies limits
  if (is.null(x_limits)) x_limits <- range(df$AI_site, na.rm = TRUE)
  if (is.null(y_limits)) y_limits <- c(if (y_floor0) 0 else NA, NA)
  
  # Build plot (note: NO scale_x limits — we only use coord_cartesian)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = AI_site, y = .data[[yvar]], fill = Aridity_Class)) +
    ggplot2::geom_point(shape = 21, color = "black", size = 2.2, alpha = 0.85, na.rm = TRUE) +
    { if (smooth)
      ggplot2::geom_smooth(method = "loess", se = TRUE, color = "black",
                           linewidth = 0.8, alpha = 0.15, na.rm = TRUE) } +
    ggplot2::scale_fill_manual(values = pal, name = "Aridity Class",
                               drop = FALSE, na.translate = FALSE) +
    ggplot2::scale_x_continuous(name = "Aridity Index (AI)", sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_y_continuous(name = ylab, sec.axis = ggplot2::dup_axis()) +
    ggplot2::labs(title = title_) +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits) +  # visual zoom (no data dropped)
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      legend.position   = "bottom",
      axis.line.x.top   = ggplot2::element_line(color = "black"),
      axis.line.y.right = ggplot2::element_line(color = "black"),
      axis.ticks.length = grid::unit(-2, "pt"),
      axis.text.x       = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text.y       = ggplot2::element_text(margin = ggplot2::margin(r = 6))
    )
  
  p
}


# Compute stats once (full-range)
gpp_stats <- compute_interannual_stats(
  annual_ai_all, flux_var = "GPP_NT_VUT_REF",
  min_years = 3, cv_abs_mean = TRUE, detrend = FALSE
)

# 1) Show full AI range (whatever your data contain)
p_sd_full <- plot_interannual_variation_vs_ai_colored(gpp_stats, kind = "sd")
print(p_sd_full)

# 2) Visual zoom to 0..1 without dropping data used for loess
p_sd_zoom01 <- plot_interannual_variation_vs_ai_colored(
  gpp_stats, kind = "sd",
  x_limits = c(0, 1)   # coord_cartesian zoom only
)
print(p_sd_zoom01)

# 3) Display only drylands (AI <= 0.65) BUT keep stats as computed
p_cv_dry <- plot_interannual_variation_vs_ai_colored(
  gpp_stats, kind = "cv",
  subset_ai = c(0, 0.65),
  x_limits  = range(gpp_stats$AI_site, na.rm = TRUE) # keep axes comparable to humid
)
print(p_cv_dry)

# 4) Display only humid (AI > 0.65) with identical axes for comparison
xlim_all <- range(gpp_stats$AI_site, na.rm = TRUE)
ylim_cv  <- c(0, NA)
p_cv_humid <- plot_interannual_variation_vs_ai_colored(
  gpp_stats, kind = "cv",
  subset_ai = c(0.65, Inf),
  x_limits  = xlim_all,
  y_limits  = ylim_cv
)
print(p_cv_humid)



# Compute stats once (full-range)
gpp_stats <- compute_interannual_stats(
  annual_ai_all, flux_var = "GPP_NT_VUT_REF",
  min_years = 3, cv_abs_mean = TRUE, detrend = FALSE
)

# 1) Show full AI range (whatever your data contain)
p_sd_full <- plot_interannual_variation_vs_ai_colored(gpp_stats, kind = "sd")
print(p_sd_full)

# 2) Visual zoom to 0..1 without dropping data used for loess
p_sd_zoom01 <- plot_interannual_variation_vs_ai_colored(
  gpp_stats, kind = "sd",
  x_limits = c(0, 1)   # coord_cartesian zoom only
)
print(p_sd_zoom01)

# 3) Display only drylands (AI <= 0.65) BUT keep stats as computed
p_cv <- plot_interannual_variation_vs_ai_colored(
  gpp_stats, kind = "cv",
  x_limits = c(0, 2.4),
  y_limits = c(-10, 100)
  )
print(p_cv)



# Compute stats once (full-range)
nee_stats <- compute_interannual_stats(
  annual_ai_all, flux_var = "NEE_VUT_REF",
  min_years = 3, cv_abs_mean = TRUE, detrend = FALSE
)

# 1) Show full AI range (whatever your data contain)
p_sd_full_NEE <- plot_interannual_variation_vs_ai_colored(nee_stats, kind = "sd")
print(p_sd_full_NEE)

# 2) Visual zoom to 0..1 without dropping data used for loess
p_sd_zoom01_NEE <- plot_interannual_variation_vs_ai_colored(
  nee_stats, kind = "sd",
  x_limits = c(0, 1)   # coord_cartesian zoom only
)
print(p_sd_zoom01_NEE)

# 3) Display only drylands (AI <= 0.65) BUT keep stats as computed
p_cv_NEE <- plot_interannual_variation_vs_ai_colored(
  nee_stats, kind = "cv",
  x_limits = c(0, 2.4),
  y_limits = c(-10, 1500)
)
print(p_cv_NEE)






# ---------- Helpers (safe if already defined elsewhere) ----------
flux_label <- function(var) {
  if (startsWith(var, "NEE"))  return(expression(NEE~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "GPP"))  return(expression(GPP~(mu*mol~m^{-2}~s^{-1})))
  if (startsWith(var, "RECO")) return(expression(Reco~(mu*mol~m^{-2}~s^{-1})))
  if (var == "LE_F_MDS")       return(expression(LE~(W~m^{-2})))
  if (var == "WUE")            return(expression(WUE==GPP/LE))
  as.name(var)
}

theme_bw_inward <- function(base_size = 14) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.position   = "bottom",
      axis.line.x.top   = ggplot2::element_line(color = "black"),
      axis.line.y.right = ggplot2::element_line(color = "black"),
      axis.ticks.length = grid::unit(-2, "pt"),
      axis.text.x       = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text.y       = ggplot2::element_text(margin = ggplot2::margin(r = 6))
    )
}

# ---------- Flux vs AI (colored by aridity class) ----------
plot_flux_vs_ai_colored <- function(
    data,
    flux_var = "GPP_NT_VUT_REF",
    ai_col   = "AI",
    site_col = "site",
    year_col = "year",
    aridity_palette = c(
      "Hyper-Arid"    = "red",
      "Arid"          = "orange",
      "Semi-Arid"     = "yellow",
      "Dry Sub-Humid" = "lightgreen",
      "Humid"         = "darkgreen"
    ),
    ai_window = c(0, 5),
    smooth = TRUE,
    site_means = FALSE,
    y_limits = NULL
) {
  stopifnot(ai_col %in% names(data))
  is_wue <- identical(flux_var, "WUE")
  
  if (!is_wue) {
    stopifnot(flux_var %in% names(data))
  } else {
    need <- c("GPP_NT_VUT_REF","LE_F_MDS")
    missing <- setdiff(need, names(data))
    if (length(missing)) stop("Missing columns for WUE: ", paste(missing, collapse = ", "))
  }
  
  ai_to_class <- function(ai) dplyr::case_when(
    is.na(ai) ~ NA_character_,
    ai < 0.05 ~ "Hyper-Arid",
    ai < 0.20 ~ "Arid",
    ai < 0.50 ~ "Semi-Arid",
    ai < 0.65 ~ "Dry Sub-Humid",
    TRUE      ~ "Humid"
  )
  
  df <- data |>
    dplyr::mutate(
      AI_val = .data[[ai_col]],
      FLUX   = if (is_wue) GPP_NT_VUT_REF / LE_F_MDS else .data[[flux_var]],
      Aridity_Class = ai_to_class(AI_val)
    ) |>
    tidyr::drop_na(AI_val, FLUX) |>
    dplyr::filter(is.finite(AI_val), is.finite(FLUX)) |>
    dplyr::filter(AI_val >= ai_window[1], AI_val <= ai_window[2])
  
  if (isTRUE(site_means)) {
    stopifnot(site_col %in% names(df))
    df <- df |>
      dplyr::group_by(.data[[site_col]]) |>
      dplyr::summarize(
        AI_val        = median(AI_val, na.rm = TRUE),
        FLUX          = mean(FLUX, na.rm = TRUE),
        Aridity_Class = ai_to_class(median(AI_val, na.rm = TRUE)),
        .groups = "drop"
      )
  }
  
  if (!nrow(df)) stop("No rows to plot after filtering; check inputs/AI window.")
  
  lvl_all <- c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid","Humid")
  df$Aridity_Class <- factor(df$Aridity_Class, levels = lvl_all)
  pal <- aridity_palette[levels(df$Aridity_Class)]
  pal[is.na(pal)] <- "grey70"; names(pal) <- levels(df$Aridity_Class)
  
  if (is.null(y_limits)) {
    nonneg <- is_wue || flux_var %in% c("GPP_NT_VUT_REF","RECO_NT_VUT_REF","LE_F_MDS")
    y_limits <- if (nonneg) c(0, NA) else c(NA, NA)
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = AI_val, y = FLUX, fill = Aridity_Class)) +
    ggplot2::geom_point(shape = 21, color = "black", size = 2.2, alpha = 0.85) +
    { if (smooth)
      ggplot2::geom_smooth(method = "loess", se = TRUE, color = "black",
                           linewidth = 0.8, alpha = 0.15) } +
    ggplot2::scale_fill_manual(values = pal, name = "Aridity Class", drop = FALSE, na.translate = FALSE) +
    ggplot2::scale_x_continuous(name = "Aridity Index (AI)", limits = c(0, 2.4), sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_y_continuous(name = if (is_wue) expression(WUE==GPP/LE) else flux_label(flux_var),
                                sec.axis = ggplot2::dup_axis()) +
    ggplot2::labs(title = paste0(flux_var, " vs Aridity Index")) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    theme_bw_inward()
}


# All sites, per-site means (cleaner plot), LOESS on
p_gpp_ai <- plot_flux_vs_ai_colored(annual_ai_all,
                                    flux_var = "GPP_NT_VUT_REF",
                                    site_means = TRUE, 
                                    ai_window = c(0, 2.4),
                                    y_limits = c(-10, 4000))
print(p_gpp_ai)

# NEE vs AI, all site-years (can be +/-), no floor at zero
p_nee_ai <- plot_flux_vs_ai_colored(annual_ai_all,
                                    flux_var = "NEE_VUT_REF",
                                    site_means = TRUE,
                                    ai_window = c(0, 2.4),
                                    y_limits = c(-1000, 1000))
print(p_nee_ai)

# LE vs AI for drylands only (AI<0.65)
p_le_dry <- plot_flux_vs_ai_colored(annual_ai_all,
                                    flux_var = "LE_F_MDS",
                                    ai_window = c(0, 2.4),
                                    site_means = TRUE)
print(p_le_dry)

# WUE vs AI, guard against LE=0/NA handled by filtering; floor at zero
p_wue <- plot_flux_vs_ai_colored(annual_ai_all,
                                 flux_var = "WUE",
                                 site_means = TRUE,
                                 y_limits = c(-10, 100))
print(p_wue)
