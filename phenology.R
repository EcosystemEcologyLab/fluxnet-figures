## Setup
library(tidyverse)
library(here)
library(ggplot2)
source("R/fcn_utility_FLUXNET.R")
source("R/detect_phenology_integral.R")
set_theme(theme_bw())

## Load data
metadata <- load_fluxnet_metadata()
manifest <-
  discover_AMF_files(data_dir = here("data/FLUXNET/AMF")) %>%
  filter(dataset == "FULLSET", time_integral == "DD") %>%
  # Deduplicate by keeping just one file per site—the one with the later end_year
  group_by(site) %>%
  filter(end_year == max(end_year)) %>% 
  ungroup()

# Feel free to replace this with whatever read function you're using.  I'd just
# recommend selecting only the site, date, and GPP columns to free up memory
# though.
flux_vars <- "GPP_NT_VUT_MEAN" # This could be a vetor

# apparently not all files have a GPP_NT_VUT_MEAN column, so uses any_of()
daily <- map2(manifest$path, manifest$site, \(path, site) {
  read_csv(
    path,
    col_select = c(TIMESTAMP, any_of(flux_vars)),
    show_col_types = FALSE
  ) %>%
    mutate(site = unique(site), .before = 1)
}) %>%
  list_rbind() %>%
  mutate(
    across(any_of(flux_vars), \(x) na_if(x, -9999)),
    date = ymd(TIMESTAMP),
    .after = site
  ) %>%
  select(-TIMESTAMP) %>%
  left_join(metadata %>% select(site, LOCATION_LAT), by = join_by(site))

daily

## Detect phenology
phenology <- daily %>%
  distinct(site, date, .keep_all = TRUE) %>%
  detect_phenology_integral(
    threshold = 0.2,
    date_var = "date", # whatever the date column is called
    flux_var = "GPP_NT_VUT_MEAN",
    keep_data = TRUE
  )

head(phenology)

## Plots
site_choice <- "US-Ha1"
## Pointrange plots
phenology %>%
  filter(site == site_choice) %>%
  mutate(flag = !is.na(issues)) %>% 
  ggplot(aes(y = year)) +
  geom_crossbar(aes(xmin = SOS, x = POS, xmax = EOS, color = flag)) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  labs(x = "DOY") 

## Timeseries plots

# All years of a specific site
phenology %>%
  filter(site == site_choice) %>% 
  unnest(data) %>% 
  ggplot(aes(x = DOY_padded)) +
  facet_wrap(vars(year)) +
  geom_point(aes(y = GPP_NT_VUT_MEAN), alpha = 0.3) +
  geom_line(aes(y = smooth), color = "blue") +
  geom_vline(aes(xintercept = SOS), color = "darkgreen", linetype = "dashed") +
  geom_vline(aes(xintercept = EOS), color = "brown", linetype = "dashed") +
  geom_vline(aes(xintercept = POS), linetype = "dashed") +
  labs(title = site_choice, x = "DOY")

# Just the smooths
phenology %>%
  filter(site == site_choice) %>%
  unnest(data) %>%
  ggplot(aes(x = DOY_padded, y = smooth, color = year, group = year)) +
  geom_line() +
  scale_color_continuous() +
  labs(title = site_choice, x = "DOY")


