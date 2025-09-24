
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
max_gapfilled_bad   <- 0.70           # >50% filled => bad
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
