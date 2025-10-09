# ---- Load source scripts (assumes they are in working directory) ----
source(file = "R/fcn_utility_FLUXNET.R")  # contains year_from_df() helper now
source(file = "R/fcn_plot_FLUXNET.R")

# If year_from_df() wasn't sourced for any reason, define a safe fallback:
if (!exists("year_from_df")) {
  year_from_df <- function(df) {
    if ("YEAR" %in% names(df)) {
      y <- suppressWarnings(as.integer(df$YEAR))
      if (any(!is.na(y))) return(y)
    }
    if ("TIMESTAMP" %in% names(df)) {
      x <- df$TIMESTAMP
      if (is.numeric(x)) {
        y <- suppressWarnings(as.integer(x))
        if (any(!is.na(y))) return(y)
      }
      if (is.character(x)) {
        y <- suppressWarnings(as.integer(substr(x, 1, 4)))
        if (any(!is.na(y))) return(y)
      }
    }
    for (cand in c("DATE", "TIMESTAMP_START", "TIMESTAMP_END")) {
      if (cand %in% names(df)) {
        x <- as.character(df[[cand]])
        y <- suppressWarnings(as.integer(substr(x, 1, 4)))
        if (any(!is.na(y))) return(y)
      }
    }
    rep(NA_integer_, nrow(df))
  }
}

# -----------------------------------------------------------------------------
# 1) Discover and load site metadata
# -----------------------------------------------------------------------------
metadata <- load_fluxnet_metadata()
## (Note: a warning about "UK" from countrycode is expected; safe to ignore.)

# -----------------------------------------------------------------------------
# 2) Discover locally available AMF/ICOS files and build a manifest
# -----------------------------------------------------------------------------
amf_files  <- discover_AMF_files(data_dir = here::here("data/FLUXNET/AMF"))
icos_files <- discover_ICOS_files(data_dir = here::here("data/FLUXNET/ICOS"))

manifest <- dplyr::bind_rows(amf_files, icos_files) %>%
  dplyr::distinct(
    site, data_product, dataset, time_integral, start_year, end_year,
    .keep_all = TRUE
  )

# -----------------------------------------------------------------------------
# 3) ICOS: Build a clean handoff table and combined ICOS annual series
#    - FULLSET (historical, often to 2014) → L2 (from first available year)
# -----------------------------------------------------------------------------

# ICOS FLUXNET2015 FULLSET (YY)
mani_FULL <- manifest %>%
  dplyr::filter(data_center == "FLX",
                data_product == "FLUXNET2015",
                time_integral == "YY",
                dataset == "FULLSET")

# ICOS L2 (YY)
mani_L2 <- manifest %>%
  dplyr::filter(data_center == "ICOSETC",
                data_product == "FLUXNET",
                time_integral == "YY",
                dataset == "L2")

# Load buckets (no cache), clean sentinels, compute year, join metadata
annual_full <- load_fluxnet_data(manifest = mani_FULL, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  dplyr::mutate(year = year_from_df(.),
                source_bucket = "FULLSET",
                network = "ICOS") %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

annual_l2 <- load_fluxnet_data(manifest = mani_L2, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  dplyr::mutate(year = year_from_df(.),
                source_bucket = "L2",
                network = "ICOS") %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

# Per-site ranges and bridge/handoff table
ranges_full <- annual_full %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(flx_min = min(year), flx_max = max(year), .groups = "drop")

ranges_l2 <- annual_l2 %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(l2_min = min(year), l2_max = max(year), .groups = "drop")

bridge <- ranges_full %>%
  dplyr::left_join(ranges_l2, by = "site") %>%
  dplyr::mutate(
    cliff_2014 = flx_max == 2014,
    has_L2     = !is.na(l2_min),
    gap_years  = dplyr::if_else(has_L2, pmax(0L, l2_min - flx_max - 1L), NA_integer_)
  )

# Clip and combine ICOS streams (avoid overlaps)
annual_full_clip <- annual_full %>%
  dplyr::inner_join(bridge %>% dplyr::select(site, flx_max), by = "site") %>%
  dplyr::filter(!is.na(year) & year <= flx_max)

annual_l2_clip <- annual_l2 %>%
  dplyr::inner_join(bridge %>% dplyr::select(site, l2_min), by = "site") %>%
  dplyr::filter(!is.na(year) & year >= l2_min)

annual_icos_combined <- dplyr::bind_rows(annual_full_clip, annual_l2_clip) %>%
  dplyr::arrange(site, year, dplyr::desc(source_bucket)) %>%  # L2 > FULLSET
  dplyr::group_by(site, year) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# Coverage tables for ICOS buckets
cov_full <- annual_full_clip %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "ICOS FULLSET")

cov_l2 <- annual_l2_clip %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "ICOS L2")

cov_comb <- annual_icos_combined %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "ICOS COMBINED")

# -----------------------------------------------------------------------------
# 4) AMF: Load FULLSET (YY)
# -----------------------------------------------------------------------------
mani_AMF_FULL <- manifest %>%
  dplyr::filter(data_center == "AMF",
                data_product == "FLUXNET",
                time_integral == "YY",
                dataset == "FULLSET")

annual_amf <- load_fluxnet_data(manifest = mani_AMF_FULL, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  dplyr::mutate(year = year_from_df(.),
                source_bucket = "AMF_FULLSET",
                network = "AMF") %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

cov_amf <- annual_amf %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "AMF FULLSET")


# --- Add FLUXNET2015 SUBSET (YY) coverage panel (for reference only) ---
mani_FLX_SUB <- manifest %>%
  dplyr::filter(data_center == "FLX",
                data_product == "FLUXNET2015",
                time_integral == "YY",
                dataset == "SUBSET")

annual_flx_subset <- load_fluxnet_data(manifest = mani_FLX_SUB, cache_file = NULL) %>%
  dplyr::mutate(across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
  dplyr::mutate(year = year_from_df(.),
                source_bucket = "FLUXNET2015_SUBSET",
                network = "GLOBAL-FLX2015") %>%
  dplyr::left_join(metadata %>% dplyr::select(-SITEID, -SITE_ID),
                   by = dplyr::join_by(site))

cov_flx_subset <- annual_flx_subset %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "FLUXNET2015 SUBSET")


# 4) Coverage tables (for plotting)
cov_full <- cov_full      # ICOS FULLSET (from earlier)
cov_l2   <- cov_l2        # ICOS L2 (from earlier)
cov_comb <- cov_comb      # ICOS COMBINED (from earlier)
cov_net  <- network_combined %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "NETWORK (stitched)")

coverage_by_year_all <- dplyr::bind_rows(
  cov_full %>% dplyr::mutate(bucket = "ICOS FULLSET"),
  cov_l2   %>% dplyr::mutate(bucket = "ICOS L2"),
  cov_amf,           # AMF FULLSET
  cov_comb %>% dplyr::mutate(bucket = "ICOS COMBINED"),
  cov_net,           # stitched network
  cov_flx_subset     # NEW: FLUXNET2015 SUBSET reference
) %>% dplyr::arrange(year, bucket)

# 5) Plot: coverage by year (faceted)
library(ggplot2)

p_cov <- ggplot(coverage_by_year_all,
                aes(x = year, y = n_site_years, group = bucket)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  labs(x = "Year",
       y = "# site-years",
       title = "Annual coverage by bucket: ICOS, AMF, Stitched Network, and FLUXNET2015 SUBSET") +
  theme_minimal(base_size = 12)

print(p_cov)
# ggsave("coverage_by_year_network_plus_subset.png", p_cov, width = 7, height = 13, dpi = 300)

# -----------------------------------------------------------------------------
# 5) STITCH NETWORK (de-dup by site,year with precedence: ICOS_L2 > ICOS_FULLSET > AMF_FULLSET)
# -----------------------------------------------------------------------------
priority_map <- c(AMF_FULLSET = 1L, FULLSET = 2L, L2 = 3L)

network_combined <- dplyr::bind_rows(
  annual_amf,
  annual_icos_combined  # already labeled with source_bucket FULLSET/L2 and network ICOS
) %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::mutate(priority = priority_map[source_bucket]) %>%
  dplyr::arrange(site, year, dplyr::desc(priority)) %>%
  dplyr::group_by(site, year) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-priority)

cov_net <- network_combined %>%
  dplyr::count(year, name = "n_site_years") %>%
  dplyr::mutate(bucket = "NETWORK (stitched)")

# -----------------------------------------------------------------------------
# 6) Coverage-by-year plot (ICOS buckets, AMF, and stitched network)
# -----------------------------------------------------------------------------
coverage_by_year_all <- dplyr::bind_rows(
  cov_full,  # ICOS FULLSET
  cov_l2,    # ICOS L2
  cov_amf,   # AMF FULLSET
  cov_comb,  # ICOS COMBINED
  cov_net    # stitched network
) %>% dplyr::arrange(year, bucket)

library(ggplot2)

p_cov <- ggplot(coverage_by_year_all,
                aes(x = year, y = n_site_years, group = bucket)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  labs(x = "Year",
       y = "# site-years",
       title = "Annual coverage by bucket: ICOS, AMF, and Stitched Network") +
  theme_minimal(base_size = 12)

print(p_cov)
# ggsave("coverage_by_year_network.png", p_cov, width = 7, height = 11, dpi = 300)

# -----------------------------------------------------------------------------
# 7) Final output table used downstream
# -----------------------------------------------------------------------------
annual <- network_combined

  
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
    gate_vars        = qc_gate_var,     # e.g. "NEE_VUT_REF"
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
    gate_vars        = qc_gate_var,     # e.g., "NEE_VUT_REF"
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


p_cov <- ggplot(coverage_by_year, aes(x = year, y = n_site_years, group = bucket)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bucket, ncol = 1, scales = "free_y") +
  labs(x = "Year", y = "# site-years", title = "ICOS annual coverage by bucket") +
  theme_minimal(base_size = 12)

print(p_cov)


##### WHAT IS GOING ON!?

# ===============================
# Coverage-by-year for ALL YY sets
# ===============================

library(dplyr)
library(purrr)
library(ggplot2)

# 1) Build the list of YY / YY_INTERIM buckets from the manifest
yy_buckets <- manifest %>%
  filter(time_integral %in% c("YY","YY_INTERIM")) %>%
  distinct(data_center, data_product, dataset, time_integral) %>%
  arrange(data_center, data_product, dataset, time_integral) %>%
  mutate(
    bucket = paste(data_center, data_product, dataset, time_integral, sep = " / ")
  )

# 2) Helper: load one bucket and return per-year site-year counts
compute_bucket_coverage <- function(dc, dp, ds, ti) {
  mani_sub <- manifest %>%
    filter(data_center == dc,
           data_product == dp,
           dataset == ds,
           time_integral == ti)
  
  if (nrow(mani_sub) == 0) {
    return(tibble(year = integer(), n_site_years = integer(),
                  data_center = dc, data_product = dp, dataset = ds, time_integral = ti))
  }
  
  dat <- load_fluxnet_data(manifest = mani_sub, cache_file = NULL) %>%
    mutate(across(where(is.numeric), ~na_if(.x, -9999))) %>%
    mutate(year = year_from_df(.)) %>%
    filter(!is.na(year))
  
  dat %>%
    count(year, name = "n_site_years") %>%
    mutate(data_center = dc,
           data_product = dp,
           dataset = ds,
           time_integral = ti)
}

# 3) Compute coverage for every bucket (map over the distinct combos)
coverage_all <- pmap_dfr(
  list(yy_buckets$data_center,
       yy_buckets$data_product,
       yy_buckets$dataset,
       yy_buckets$time_integral),
  compute_bucket_coverage
) %>%
  mutate(bucket = paste(data_center, data_product, dataset, time_integral, sep = " / "))

# (optional) order facets by total site-years to make the plot easier to read
bucket_order <- coverage_all %>%
  group_by(bucket) %>%
  summarise(total_sy = sum(n_site_years), .groups = "drop") %>%
  arrange(desc(total_sy)) %>%
  pull(bucket)

coverage_all <- coverage_all %>%
  mutate(bucket = factor(bucket, levels = bucket_order))

# 4) Plot
p_all <- ggplot(coverage_all, aes(x = year, y = n_site_years, group = bucket)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bucket, ncol = 2, scales = "free_y") +
  labs(x = "Year",
       y = "# site-years",
       title = "Annual coverage by (data center × product × dataset × integral) — YY & YY_INTERIM") +
  theme_minimal(base_size = 12)

print(p_all)
# ggsave("coverage_by_year_ALL_YY.png", p_all, width = 14, height = 16, dpi = 300)



library(dplyr)
library(ggplot2)
library(purrr)

# Helper to compute coverage from an already-loaded data frame
coverage_from_df <- function(df, label) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  if (!"NEE_VUT_REF" %in% names(df)) return(tibble())   # skip buckets without NEE
  df %>%
    filter(!is.na(year)) %>%
    count(year, name = "n_site_years") %>%
    mutate(bucket = label)
}

# Build a list of the already-loaded buckets you care about
preloaded <- list(
  "ICOS FULLSET"          = annual_full,
  "ICOS L2"               = annual_l2,
  "AMF FULLSET"           = annual_amf
  # If you made this earlier and want to show it too, uncomment:
  # "FLUXNET2015 SUBSET"    = annual_flx_subset
)

coverage_by_year_NEI <- imap_dfr(preloaded, ~ coverage_from_df(.x, .y))

# Order facets by total site-years
bucket_order <- coverage_by_year_NEI %>%
  group_by(bucket) %>% summarise(total_sy = sum(n_site_years), .groups="drop") %>%
  arrange(desc(total_sy)) %>% pull(bucket)

coverage_by_year_NEI <- coverage_by_year_NEI %>%
  mutate(bucket = factor(bucket, levels = bucket_order))

# Plot
p_cov_nee <- ggplot(coverage_by_year_NEI,
                    aes(x = year, y = n_site_years, group = bucket)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bucket, ncol = 2, scales = "free_y") +
  labs(x = "Year", y = "# site-years",
       title = "Annual coverage (only buckets containing NEE_VUT_REF)") +
  theme_minimal(base_size = 12)

print(p_cov_nee)


library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)

# 1) Slice the manifest to YY + YY_INTERIM, keep one representative file per bucket
yy_manifest <- manifest %>%
  filter(time_integral %in% c("YY", "YY_INTERIM")) %>%
  mutate(bucket = paste(data_center, data_product, dataset, time_integral, sep = " / "))

# choose a representative path per bucket (first is fine)
reps <- yy_manifest %>%
  group_by(data_center, data_product, dataset, time_integral, bucket) %>%
  summarise(
    n_files   = n(),
    example_path = first(path),
    example_file = basename(example_path),
    .groups = "drop"
  )

# 2) Lightweight header check: does this bucket's example file contain NEE_VUT_REF?
check_has_nee <- function(p) {
  out <- tryCatch(
    {
      hdr <- readr::read_csv(p, n_max = 0, show_col_types = FALSE)
      list(has_nee = "NEE_VUT_REF" %in% names(hdr), note = NA_character_)
    },
    error = function(e) list(has_nee = NA, note = paste("read error:", e$message))
  )
  tibble(has_NEE_VUT_REF = out$has_nee, note = out$note)
}

nee_results <- reps$example_path %>% map_dfr(check_has_nee)

# 3) Assemble the table
yy_inventory <- reps %>%
  bind_cols(nee_results) %>%
  select(
    bucket,                        # dataset "name" (data_center / product / dataset / integral)
    example_file,                  # indicative filename
    n_files,                       # how many files in this bucket on disk
    has_NEE_VUT_REF,
    note
  ) %>%
  arrange(bucket)

# 4) Show it (and optionally write it out)
print(yy_inventory, n = nrow(yy_inventory))
 readr::write_csv(yy_inventory, "yy_inventory_with_nee_flag.csv")



