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


library(stringr)
# --- 1) Per-site coverage ---
site_coverage <- annual %>%
  group_by(site) %>%
  summarise(
    n_years   = n_distinct(year, na.rm = TRUE),
    last_year = suppressWarnings(max(year, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(is.finite(last_year), n_years > 0) %>%
  mutate(
    # Num of years bins (interpret "<5" as <=5)
    num_years_bin = case_when(
      n_years <= 5           ~ "<5",
      n_years <= 10          ~ "6–10",
      n_years <= 15          ~ "11–15",
      n_years <= 20          ~ "16–20",
      TRUE                   ~ "≥21"
    ),
    # Last-year bins
    last_bin = case_when(
      last_year < 2023       ~ "last <2023",
      last_year == 2023      ~ "last 2023*",
      last_year >= 2024      ~ "last 2024*"
    )
  )

# --- 2) Your Group1/Group2 co-author scheme as a lookup table ---
scheme <- tribble(
  ~num_years_bin, ~last_bin,       ~g1, ~g2,
  "<5",           "last <2023",     1,   1,
  "<5",           "last 2023*",     1,   2,
  "<5",           "last 2024*",     1,   3,
  "6–10",         "last <2023",     1,   3,
  "6–10",         "last 2023*",     1,   4,
  "6–10",         "last 2024*",     1,   5,
  "11–15",        "last <2023",     2,   2,
  "11–15",        "last 2023*",     2,   3,
  "11–15",        "last 2024*",     2,   4,
  "16–20",        "last <2023",     2,   4,
  "16–20",        "last 2023*",     2,   5,
  "16–20",        "last 2024*",     2,   6,
  "≥21",          "last <2023",     3,   3,
  "≥21",          "last 2023*",     3,   4,
  "≥21",          "last 2024*",     3,   5
)

# --- 3) Assign per-site co-author counts (Group1 + Group2) ---
site_authors <- site_coverage %>%
  left_join(scheme, by = c("num_years_bin", "last_bin")) %>%
  mutate(
    coauthors_per_site = g1 + g2,
    a_plus_b_label     = sprintf("%d + %d = %d", g1, g2, coauthors_per_site)
  )

# Safety check: all sites should match a scheme row
stopifnot(!any(is.na(site_authors$g1) | is.na(site_authors$g2)))

# --- 4) Cell-level counts and totals (how many sites, total authors) ---
cell_summary <- site_authors %>%
  count(num_years_bin, last_bin, name = "n_sites") %>%
  left_join(scheme, by = c("num_years_bin", "last_bin")) %>%
  mutate(
    coauthors_per_site = g1 + g2,
    total_authors_cell = n_sites * coauthors_per_site,
    label = sprintf("%s • %s: %d sites × (%d + %d = %d) = %d",
                    num_years_bin, last_bin, n_sites, g1, g2,
                    coauthors_per_site, total_authors_cell)
  ) %>%
  arrange(
    factor(num_years_bin, levels = c("<5","6–10","11–15","16–20","≥21")),
    factor(last_bin, levels = c("last <2023","last 2023*","last 2024*"))
  )

# --- 5) Grand totals (across all sites/cells) ---
totals <- cell_summary %>%
  summarise(
    total_sites            = sum(n_sites),
    total_group1_authors   = sum(n_sites * g1),
    total_group2_authors   = sum(n_sites * g2),
    total_authors_overall  = sum(total_authors_cell),
    .groups = "drop"
  )

# --- 6) Optional: a wide, doc-ready table with a readable cell string ---
wide_counts <- cell_summary %>%
  transmute(
    num_years_bin, last_bin,
    display = sprintf("%d sites: (%d + %d = %d) → %d total",
                      n_sites, g1, g2, coauthors_per_site, total_authors_cell)
  ) %>%
  pivot_wider(names_from = last_bin, values_from = display) %>%
  arrange(factor(num_years_bin, levels = c("<5","6–10","11–15","16–20","≥21"))) %>%
  rename(`Num of years` = num_years_bin)

# --- Outputs you can inspect --- 
site_authors   # one row per site with its a+b and label
cell_summary   # per cell: #sites and total authors implied by your rules
totals         # network totals (Group1, Group2, and overall)
wide_counts    # pretty table, easy to paste into docs