# # FLUXNET2015 workflow
# 
# Requires that TimeSeriesPlots_fluxnet_fullrecord be already run
# 

# #add global metadata
# Fluxnet_info <- read.csv("../fluxnetreader/data/FLUXNET_INFO/FLUXNET10282025_INFO.csv")
# Fluxnet_info <- Fluxnet_info %>%
#   rename(site = SITEID)

# # Read SUBSET_YY files (in data folder only)
annual_sites_paths_FLUXNET2015 <- list.files(
  path       = "data",
  pattern    = paste0("(", paste(FLUXNET2015_FULLSET_patterns, collapse = "|"), ").*\\.csv$"),
  recursive  = TRUE,          # ← look in all subdirectories
  full.names = TRUE
)
# 
# # Robust processing: extract site ID using string manipulation
multiple_sites_annual_FLUXNET2015 <- map_df(
  annual_sites_paths_FLUXNET2015,
  function(path) {
    site_id <- str_split(basename(path), "_", simplify = TRUE)[, 2]
    df <- read_csv(path)
    df$site <- site_id
    return(df)
  }
)

# 1) pull out unique site‐lists
sites_main <- unique(multiple_sites_annual$site)
sites_FLUXNET2015  <- unique(multiple_sites_annual_FLUXNET2015$site)

# 2) find shared vs only‐in‐one
shared_sites  <- intersect(sites_main, sites_FLUXNET2015)
only_ICOS_AMF     <- setdiff(sites_main, sites_FLUXNET2015)
only_FLUXNET2015   <- setdiff(sites_FLUXNET2015, sites_main)

cat("Number of shared sites:     ", length(shared_sites), "\n")
cat("Only in ICOS AMF annual:        ", length(only_ICOS_AMF), "\n")
cat("Only in FLUXNET 2015 annual:      ", length(only_FLUXNET2015), "\n")

# 3) summarize first‐year & length for shared sites in the main dataset
summary_ICOS_AMF <- multiple_sites_annual %>%
  filter(site %in% shared_sites) %>%
  group_by(site) %>%
  summarize(
    start_ICOS_AMF  = min(TIMESTAMP, na.rm = TRUE),
    length_ICOS_AMF = n_distinct(TIMESTAMP),
    .groups     = "drop"
  )

# 4) same for the subset dataset
summary_FLUXNET2015 <- multiple_sites_annual_FLUXNET2015 %>%
  filter(site %in% shared_sites) %>%
  group_by(site) %>%
  summarize(
    start_FLUXNET2015   = min(TIMESTAMP, na.rm = TRUE),
    length_FLUXNET2015  = n_distinct(TIMESTAMP),
    .groups     = "drop"
  )

# 5) join those two summaries into one table
shared_summary <- summary_ICOS_AMF %>%
  left_join(summary_FLUXNET2015, by = "site")

# Now you can inspect `shared_summary`:
print(shared_summary)


# 1) grab only the subset rows with site‐year NOT in the main dataset
to_add <- multiple_sites_annual_FLUXNET2015 %>%
  anti_join(
    multiple_sites_annual,
    by = c("site", "TIMESTAMP")
  )

# 2) bind them onto the main table
combined_annual <- bind_rows(
  multiple_sites_annual,
  to_add
) %>%
  arrange(site, TIMESTAMP)

# 3) sanity‐check: each (site, year) should now be unique
combined_annual %>%
  count(site, TIMESTAMP) %>%
  filter(n > 1)   # should return zero rows

# And a quick summary of your new total record lengths:
combined_annual %>%
  group_by(site) %>%
  summarize(n_years = n_distinct(TIMESTAMP), .groups = "drop")


# 6) calculate end years for each summary table
shared_summary_gaps <- shared_summary %>%
  mutate(
    end_FLUXNET2015 = start_FLUXNET2015 + length_FLUXNET2015 - 1,
    gap_start       = end_FLUXNET2015 + 1,
    gap_end         = start_ICOS_AMF - 1,
    gap_years_count = gap_end - gap_start + 1
  ) %>%
  # keep only sites with an actual gap
  filter(gap_years_count > 0)

# Inspect sites with gaps
print(shared_summary_gaps)
#> # A tibble: … × 8
#>    site  start_ICOS_AMF length_ICOS_AMF start_FLUXNET2015 length_FLUXNET2015 end_FLUXNET2015 gap_start gap_end gap_years_count
#>    <chr>          <dbl>           <int>             <dbl>              <int>            <dbl>     <dbl>   <dbl>           <dbl>
#> 1 BE-Bra          2020               5              1996                 19             2014      2015    2019               5
#> 2 …


# 7) (Optional) expand into one row per missing year
missing_years <- shared_summary_gaps %>%
  mutate(
    missing_year = map2(gap_start, gap_end, seq)
  ) %>%
  unnest(missing_year)

# See the exact missing years per site
print(missing_years)

# pull the FLUXNET2015‐only list
sites_FLUXNET2015 <- unique(multiple_sites_annual_FLUXNET2015$site)

# find sites with any missing years
sites_missing <- combined_annual %>%
  filter(site %in% sites_FLUXNET2015) %>% 
  group_by(site) %>%
  summarize(
    first_year     = min(TIMESTAMP, na.rm = TRUE),
    last_year      = max(TIMESTAMP, na.rm = TRUE),
    n_years        = n_distinct(TIMESTAMP),
    expected_years = last_year - first_year + 1,
    missing_years  = expected_years - n_years,
    .groups        = "drop"
  ) %>%
  filter(missing_years > 0) %>%
  pull(site)

length(sites_missing)

combined_annual <- combined_annual %>% 
  mutate(replace(across(cols_to_check), across(cols_to_check) < -9000, NA))
