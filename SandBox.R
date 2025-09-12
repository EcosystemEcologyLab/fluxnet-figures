
source(file="R/fcn_utility_FLUXNET.R")
source(file="R/fcn_plot_FLUXNET.R")

library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(ggplot2)
library(here)
# uses your existing functions:
# - discover_AMF_files()
# - discover_ICOS_files()
# - filter_manifest()
# - load_fluxnet_data()
metadata <- load_fluxnet_metadata()
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `COUNTRY = coalesce(...)`.
## Caused by warning:
## ! Some values were not matched unambiguously: UK
amf_files <- discover_AMF_files(data_dir = here::here("data/FLUXNET/AMF"))
## • AUX / AUXMETEO → 12 sites, 130 site-years across 12 files
## • AUX / AUXNEE → 12 sites, 130 site-years across 12 files
## • DD / ERA5 → 12 sites, 517 site-years across 12 files
## • DD / FULLSET → 299 sites, 2177 site-years across 301 files
## • HH / ERA5 → 12 sites, 517 site-years across 12 files
## • HH / FULLSET → 282 sites, 2011 site-years across 284 files
## • MM / ERA5 → 12 sites, 517 site-years across 12 files
## • MM / FULLSET → 299 sites, 2177 site-years across 301 files
## • WW / ERA5 → 12 sites, 517 site-years across 12 files
## • WW / FULLSET → 12 sites, 130 site-years across 12 files
## • YY / ERA5 → 12 sites, 517 site-years across 12 files
## • YY / FULLSET → 299 sites, 2177 site-years across 301 files
icos_files <- discover_ICOS_files(data_dir = here::here("data/FLUXNET/ICOS"))
## • DD / L2 → 70 sites, 0 total site-years across 70 files
## • HH / L2 → 1 sites, 0 total site-years across 1 files
## • MM / L2 → 70 sites, 0 total site-years across 70 files
## • WW / L2 → 1 sites, 0 total site-years across 1 files
## • YY / L2 → 70 sites, 0 total site-years across 70 files
manifest <- bind_rows(amf_files, icos_files)

# deduplicate manifest?
manifest <- manifest %>%
  distinct(
    site,
    data_product,
    dataset,
    time_integral,
    start_year,
    end_year,
    .keep_all = TRUE
  )
annual <- manifest %>%
  filter(time_integral == "YY", dataset == "FULLSET") %>%
  load_fluxnet_data() %>%
  mutate(across(where(is.numeric), \(x) na_if(x, -9999))) %>%
  mutate(year = as.integer(TIMESTAMP), .before = TIMESTAMP) %>%
  left_join(metadata %>% select(-SITEID, -SITE_ID), by = join_by(site))
## → Will read 383 file(s) with integrals: YY  (sites: ALL)

hist(annual$NEE_VUT_REF)



plots_igbp <- plot_flux_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(plots_igbp$composite_plot)


plots_slice <- plot_flux_by_igbp_timeslice_grouped(annual, flux_var = "NEE_VUT_REF")
print(plots_slice$flux_plot)


plots_group <- plot_flux_box_by_group(annual, flux_var = "NEE_VUT_REF", y_mode = "squish")
print(plots_group$Forest)

ts_nee <- plot_flux_timeseries_by_igbp(annual, flux_var = "NEE_VUT_REF")
print(ts_nee)


