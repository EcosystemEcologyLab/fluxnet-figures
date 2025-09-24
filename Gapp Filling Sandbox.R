
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
  
# -----------------------------------------------------------------------------
# 4) Gate by "percent gap-filled" at the annual scale
#    - We use the *_QC that accompanies your gating variable at YY.
#    - For NEE: NEE_VUT_REF_QC in [0..1]. We interpret (1 - QC) as % gap-filled.
#    - Default rule: "bad" if > 50% gap-filled (you can change this easily).
# -----------------------------------------------------------------------------

# ---- user-tunable knobs ----
qc_gate_var         <- "NEE_VUT_REF"  # e.g., "NEE_VUT_REF", "GPP_NT_VUT_REF", "LE_F", etc.
max_gapfilled_bad   <- 0.50           # >50% filled => bad
drop_if_qc_missing  <- TRUE           # if QC missing at YY, drop the record

# change the rule: set max_gapfilled_bad <- 0.25 (require ≥75% “good”).
# Gate on GPP instead of NEE: qc_gate_var <- "GPP_NT_VUT_REF".
# Keep records with missing QC: set drop_if_qc_missing <- FALSE.

# helper that adds pct_gapfilled + is_bad, based on the YY QC column for the chosen var
flag_bad_gapfilled <- function(df, gate_var, max_gapfilled = 0.5, drop_if_missing = TRUE) {
  qc_col <- paste0(gate_var, "_QC")
  if (!qc_col %in% names(df)) {
    warning(sprintf("QC column `%s` not found. No filtering applied.", qc_col))
    df$pct_gapfilled <- NA_real_
    df$is_bad <- if (drop_if_missing) TRUE else FALSE
    return(df)
  }
  pg <- df[[qc_col]]                        # fraction "good" at YY (0..1)
  df$pct_gapfilled <- pmax(0, pmin(1, 1 - pg))  # clamp just in case
  df$is_bad <- ifelse(
    is.na(df$pct_gapfilled),
    drop_if_missing,                        # drop if we can't assess quality
    df$pct_gapfilled > max_gapfilled
  )
  df
}

annual_flagged <- annual |>
  flag_bad_gapfilled(
    gate_var        = qc_gate_var,
    max_gapfilled   = max_gapfilled_bad,
    drop_if_missing = drop_if_qc_missing
  )

# keep the good stuff; also keep a small audit table of exclusions
annual_clean <- annual_flagged |>
  dplyr::filter(!is_bad)

annual_excluded <- annual_flagged |>
  dplyr::filter(is_bad) |>
  dplyr::select(site, year, pct_gapfilled,
                dplyr::all_of(paste0(qc_gate_var, "_QC")),
                dplyr::any_of(c(qc_gate_var, paste0(qc_gate_var, "_JOINTUNC"))))


library(dplyr)
library(ggplot2)
library(purrr)
library(scales)

thresholds <- c(0, 0.25, 0.50, 0.75, 1.00)

# For each threshold:
#  - flag "bad" site-years
#  - keep only good ones
#  - compute: (a) per-year mean NEE and per-year mean %gapfilled
#             (b) overall mean NEE and overall mean %gapfilled (for the sensitivity point)
per_thr <- map_dfr(thresholds, function(thr) {
  df_flag <- flag_bad_gapfilled(
    annual,
    gate_var        = qc_gate_var,     # e.g. "NEE_VUT_REF"
    max_gapfilled   = thr,
    drop_if_missing = TRUE
  )
  
  kept <- df_flag %>% filter(!is_bad)
  
  # per-year summary (for the timeline plot)
  by_year <- kept %>%
    summarise(
      avg_NEE = mean(NEE_VUT_REF, na.rm = TRUE),
      avg_pct_gapfilled = mean(pct_gapfilled, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(threshold = thr)
  
  # overall summary (for the sensitivity plot)
  overall <- kept %>%
    summarise(
      mean_NEE_overall = mean(NEE_VUT_REF, na.rm = TRUE),
      mean_pct_gapfilled_overall = mean(pct_gapfilled, na.rm = TRUE)
    ) %>%
    mutate(threshold = thr)
  
  bind_rows(
    by_year %>% mutate(.type = "by_year"),
    overall  %>% mutate(.type = "overall")
  )
})

# ---- Plot 1: Sensitivity curve (x = actual % gap-filled, one point per threshold) ----
sens_df <- per_thr %>% filter(.type == "overall")

ggplot(sens_df,
       aes(x = mean_pct_gapfilled_overall, y = mean_NEE_overall,
           label = percent(threshold))) +
  geom_line() +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(min.segment.length = 0) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "% gap-filled (actual, among retained site-years)",
    y = expression("Mean annual NEE (gC "*m^{-2}*" "*y^{-1}*")"),
    title = "Sensitivity of mean annual NEE to gap-filled fraction",
    subtitle = "Each point summarizes the dataset retained under a given max % gap-filled rule"
  ) +
  theme_minimal(base_size = 12)

# ---- Plot 2: Single timeline, colored by threshold (x = Year; color/shape = threshold) ----# Count number of sites per year and threshold
library(dplyr)
library(ggplot2)
library(purrr)
library(scales)

thresholds <- c(0, 0.25, 0.50, 0.75, 1.00)

# Rebuild the per-threshold table so it keeps site info long enough to count sites
per_thr <- map_dfr(thresholds, function(thr) {
  df_flag <- flag_bad_gapfilled(
    annual,
    gate_var        = qc_gate_var,     # e.g., "NEE_VUT_REF"
    max_gapfilled   = thr,
    drop_if_missing = TRUE
  )
  
  kept <- df_flag %>% filter(!is_bad)
  
  # ----- per-year summary (retain site counts!) -----
  by_year <- kept %>%
    summarise(
      avg_NEE            = mean(NEE_VUT_REF, na.rm = TRUE),
      n_sites_retained   = n_distinct(site),
      avg_pct_gapfilled  = mean(pct_gapfilled, na.rm = TRUE),
      .by = year
    ) %>%
    mutate(threshold = thr, .type = "by_year")
  
  # ----- overall summary for sensitivity plot -----
  overall <- kept %>%
    summarise(
      mean_NEE_overall           = mean(NEE_VUT_REF, na.rm = TRUE),
      mean_pct_gapfilled_overall = mean(pct_gapfilled, na.rm = TRUE),
      n_sites_overall            = n_distinct(site)
    ) %>%
    mutate(threshold = thr, .type = "overall")
  
  bind_rows(by_year, overall)
})

# ---------------- Plot 2: timeline with secondary axis for #sites ----------------
yr_df <- per_thr %>%
  filter(.type == "by_year") %>%
  mutate(threshold_f = factor(threshold, levels = thresholds, labels = percent(thresholds)))

# scale secondary axis cleanly
rng_nee   <- range(yr_df$avg_NEE, na.rm = TRUE)
max_sites <- max(yr_df$n_sites_retained, na.rm = TRUE)
site_scale <- if (is.finite(rng_nee[2]) && is.finite(max_sites) && max_sites > 0) rng_nee[2] / max_sites else 1

ggplot(yr_df, aes(x = year)) +
  # NEE (primary y)
  geom_line(aes(y = avg_NEE, color = threshold_f, group = threshold_f), alpha = 0.7) +
  geom_point(aes(y = avg_NEE, color = threshold_f, shape = threshold_f), size = 2) +
  # #sites (secondary y), drawn per threshold so you see losses as filtering tightens
  geom_line(aes(y = n_sites_retained * site_scale, color = threshold_f, group = threshold_f),
            linetype = "dashed", alpha = 0.6) +
  geom_point(aes(y = n_sites_retained * site_scale, color = threshold_f),
             size = 1.6, alpha = 0.6) +
  scale_y_continuous(
    name = expression("Mean annual NEE (gC "*m^{-2}*" "*y^{-1}*")"),
    sec.axis = sec_axis(~ . / site_scale, name = "# of sites retained")
  ) +
  labs(
    x = "Year",
    color = "Max % gap-filled\n(threshold)",
    shape = "Max % gap-filled\n(threshold)",
    title = "Mean annual NEE and # of Sites Retained by Threshold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y.right = element_text(color = "grey30"),
    axis.text.y.right  = element_text(color = "grey30")
  )
