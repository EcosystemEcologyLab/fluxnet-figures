#' ---
#' title: "Initial FLUXNET Plots"
#' author: "Kristina Riemer & Dave Moore"
#' date: "`r Sys.Date()`"
#' format:
#'   html:
#'     toc: true
#'     toc-expand: 2
#'     toc-location: left
#'     css: styles.css
#' ---
#' 
#' ## TODO list
#' 
#' - Data
#'   - Download data for more sites
#'   - Automated checking of data ranges? 
#' - Plots
#'   - Scale by number of years for each date
#'   - Do all initial plots
#'   - Change IGBP codes to definitions
#'   - Add overall trend line to interannual met variability plots
#'   - Facet all figures for interannual met variability
#' 
#' ## Data
#' ### Install libraries {.unnumbered .unlisted}
#' 
#' These are not on CRAN so require downloading directly from their GitHub repos using `devtools`. This code chunk only needs to be run once (in a while) and it could take a while depending on if you choose to update all the dependencies. 
#' 
#' ::: {.callout-note}
#' Need dev version of `amerifluxr` package to get `amf_download_fluxnet` function. 
#' :::
#' 
## -------------------------------------------------
#| eval: false

# devtools::install_github("chuhousen/amerifluxr")
# devtools::install_github("valentinitnelav/plotbiomes")

#' 
#' ### Read in libraries {.unnumbered .unlisted}
#' 
## -------------------------------------------------
#| output: false
library(dplyr)
library(lubridate)
library(ggplot2)
library(amerifluxr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)
library(plotbiomes)
library(measurements)
library(ggnewscale)

#' 
#' ### Download site metadata {.unnumbered .unlisted}
#' 
#' Showing what metadata is available for a site
#' 
## -------------------------------------------------
site_metadata <- amf_site_info()
site_metadata %>% 
  filter(SITE_ID == "US-SRM")

#' Do download setup
#' 
#' - Increase timeout because default of one minute isn't enough
#' 
## -------------------------------------------------
if (!dir.exists("data")){
  dir.create("data")
}

options(timeout = 600)

#' 
#' ### Download individual site data {.unnumbered .unlisted}
#' 
#' Demonstrating how to use `amf_download_fluxnet` function
#' 
## -------------------------------------------------
#| eval: false

# amf_download_fluxnet(user_id = "kristinariemer",
#                      user_email = "kristinariemer@arizona.edu",
#                      site_id = "US-xSR",
#                      data_product = "FLUXNET",
#                      data_variant = "FULLSET",
#                      data_policy = "CCBY4.0",
#                      agree_policy = TRUE,
#                      intended_use = "synthesis",
#                      intended_use_text = "creating pipeline for standardized figures",
#                      out_dir = "data/")
# unzip("data/AMF_US-xSR_FLUXNET_FULLSET_2017-2021_3-5.zip")

#' 
#' ### Download multiple sites data {.unnumbered .unlisted}
#' 
#' This is for all of the Arizona sites, which includes all the sites in Dave's list. Gets list of sites that already have data downloaded (`downloaded_sites`). Then goes through each Arizona site and downloads the data if needed. If only BASE data is available, it returns the message `Cannot find data from [site]`. Downloads the zip file and unzips it. 
#' 
## -------------------------------------------------
#| eval: false

# az_sites <- site_metadata %>%
#   filter(STATE == "AZ")
# 
# downloaded_sites <- list.dirs("data") %>%
#   str_split_i("_", 2)
# 
# for(site in az_sites$SITE_ID){
#   if(!site %in% downloaded_sites){
#     print(paste0("Downloading data for ", site))
#     tryCatch({
#       amf_download_fluxnet(user_id = "kristinariemer",
#                            user_email = "kristinariemer@arizona.edu",
#                            site_id = site,
#                            data_product = "FLUXNET",
#                            data_variant = "FULLSET",
#                            data_policy = "CCBY4.0",
#                            agree_policy = TRUE,
#                            intended_use = "synthesis",
#                            intended_use_text = "creating pipeline for standardized figures",
#                            out_dir = "data/")
#       zip_path <- Sys.glob(file.path("data", paste0("AMF_", site, "*")))
#       unzipped_path <- tools::file_path_sans_ext(zip_path)
#       unzip(zip_path, exdir = unzipped_path)
#       }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")})
#     } else {
#       print(paste0("Data has already been downloaded for ", site))
#     }
# }

#' 
#' ### Read in data {.unnumbered .unlisted}
#' 
#' Single site details
#' 
#' - Site: [US-SRM](https://ameriflux.lbl.gov/sites/siteinfo/US-SRM) (Santa Rita Mesquite)
#' - Years: 2004 - 2023
#' - IGBP: Woody Savannas
#' 
## -------------------------------------------------
single_site_daily <- read.csv("data/AMF_US-SRM_FLUXNET_FULLSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_FULLSET_DD_2004-2023_3-6.csv")
single_site_hourly <- read.csv("data/AMF_US-SRM_FLUXNET_FULLSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_FULLSET_HH_2004-2023_3-6.csv")
single_site_annually <- read.csv("data/AMF_US-SRM_FLUXNET_FULLSET_2004-2023_3-6/AMF_US-SRM_FLUXNET_FULLSET_YY_2004-2023_3-6.csv")

#' 
#' Multiple sites
#' 
#' They used many different variants of -9999 to represent null values. 
#' 
## -------------------------------------------------
#| output: false

daily_sites_paths <- list.files(".", pattern = "(FLUXNET_FULLSET_DD)", recursive = TRUE) 

multiple_sites_daily <- map_df(daily_sites_paths, ~read_csv(.x) %>% mutate(file = basename(.x), .before = 1)) %>% 
  mutate(file = str_split_i(file, "_", 2)) %>% 
  rename(site = file)

annual_sites_paths <- list.files(".", pattern = "(FLUXNET_FULLSET_YY)", recursive = TRUE) 

multiple_sites_annual <- map_df(annual_sites_paths, ~read_csv(.x) %>% mutate(file = basename(.x), .before = 1)) %>% 
  mutate(file = str_split_i(file, "_", 2)) %>% 
  rename(site = file)

#' 
#' ### Change null values to NA {.unnumbered .unlisted}
#' 
#' Null values in FLUXNET are indicated by some variation of -9999 (-9999.x, where x can be multiple values of 0 or 9). See **Missing data** section on [Data Variables page](https://fluxnet.org/data/aboutdata/data-variables/). Returning datasets for daily and annual data that contain these NA values and then remove them from the datasets. 
#' 
## -------------------------------------------------
#| warning: false
cols_to_check <- c("GPP_NT_VUT_REF", "RECO_NT_VUT_REF", "NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")

multiple_sites_annual_nulls <- multiple_sites_annual %>% 
  select(site, TIMESTAMP, cols_to_check) %>% 
  filter(if_any(cols_to_check, ~ . < -9000))

multiple_sites_annual <- multiple_sites_annual %>% 
  mutate(replace(across(cols_to_check), across(cols_to_check) < -9000, NA))

multiple_sites_daily_nulls <- multiple_sites_daily %>% 
  select(site, TIMESTAMP, cols_to_check) %>% 
  filter(if_any(cols_to_check, ~ . < -9000))

multiple_sites_daily <- multiple_sites_daily %>% 
  mutate(replace(across(cols_to_check), across(cols_to_check) < -9000, NA))

#' 
#' The `{r} length(unique(multiple_sites_daily$site)) ` sites currently in the report are: `{r} unique(multiple_sites_daily$site)`
#' 
#' ## Figures
#' 
#' Find all variables described on the [FULLSET Data Product page](https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/). Plots are generally of 3 variables: 
#' 
#' 1. GPP_NT_VUT_REF
#' 2. RECO_NT_VUT_REF
#' 3. NEE_VUT_REF
#' 
#' ### Entire time series (single site)
#' 
#' Lots of ways to smooth time series (i.e., filter)
#' 
#' - Simple running mean from [this book chapter](https://cswr.nrhstat.org/timeseries)
#' - Takes mean of value and (n-1) / 2 values on either side of it
#' - Bigger window size = more averaging
#' 
## -------------------------------------------------
window_size <- 51

#' 
## -------------------------------------------------
#| code-fold: true
#| warning: false

total_ts_gpp <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         running_mean_gpp = stats::filter(GPP_NT_VUT_REF, rep(1/window_size, window_size)))

yr_start_dates <- total_ts_gpp %>% 
  select(date_object) %>% 
  filter(grepl("-01-01", date_object)) %>% 
  pull()

ggplot(total_ts_gpp, aes(x = date_object, y = GPP_NT_VUT_REF)) + 
  geom_point(size = 0.5, color = "darkgrey") +
  geom_line(aes(y = running_mean_gpp), color = "blue", lwd = 1) +
  geom_vline(xintercept = yr_start_dates, color = "grey", alpha = 0.5) + 
  labs(x = "Date", y = "GPP (smoothed)") +
  theme_classic()

total_ts_reco <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         running_mean_reco = stats::filter(RECO_NT_VUT_REF, rep(1/window_size, window_size)))

ggplot(total_ts_reco, aes(x = date_object, y = RECO_NT_VUT_REF)) + 
  geom_point(size = 0.5, color = "darkgrey") +
  geom_line(aes(y = running_mean_reco), color = "blue", lwd = 1) +
  geom_vline(xintercept = yr_start_dates, color = "grey", alpha = 0.5) + 
  labs(x = "Date", y = "RECO (smoothed)") +
  theme_classic()

total_ts_nee <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         running_mean_nee = stats::filter(NEE_VUT_REF, rep(1/window_size, window_size)))

ggplot(total_ts_nee, aes(x = date_object, y = NEE_VUT_REF)) + 
  geom_point(size = 0.5, color = "darkgrey") +
  geom_line(aes(y = running_mean_nee), color = "blue", lwd = 1) +
  geom_vline(xintercept = yr_start_dates, color = "grey", alpha = 0.5) + 
  labs(x = "Date", y = "NEE (smoothed)") +
  theme_classic()

#' 
#' ### Average annual time series (single site)
#' 
#' Show average daily values (with standard deviations)
#' 
## -------------------------------------------------
#| code-fold: true
gpp_by_date <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         date_minus_year = format(date_object, '%m-%d')) %>% 
  group_by(date_minus_year) %>% 
  summarize(gpp_mean = mean(GPP_NT_VUT_REF), 
            gpp_sd = sd(GPP_NT_VUT_REF)) %>% 
  mutate(date_fake_year = ymd(paste0("2024-", date_minus_year)))

ggplot(gpp_by_date, aes(x = date_fake_year, y = gpp_mean)) +
  geom_ribbon(aes(ymax = gpp_mean + gpp_sd, ymin = gpp_mean - gpp_sd), 
              fill = "grey") + 
    geom_point() +
    labs(x = "Date",
         y = "Mean GPP +/- SD") +
    theme_minimal() + 
  scale_x_date(date_labels = "%B")

reco_by_date <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         date_minus_year = format(date_object, '%m-%d')) %>% 
  group_by(date_minus_year) %>% 
  summarize(reco_mean = mean(RECO_NT_VUT_REF), 
            reco_sd = sd(RECO_NT_VUT_REF)) %>% 
  mutate(date_fake_year = ymd(paste0("2024-", date_minus_year)))

ggplot(reco_by_date, aes(x = date_fake_year, y = reco_mean)) +
  geom_ribbon(aes(ymax = reco_mean + reco_sd, ymin = reco_mean - reco_sd), 
              fill = "grey") + 
    geom_point() +
    labs(x = "Date",
         y = "Mean RECO +/- SD") +
    theme_minimal() + 
  scale_x_date(date_labels = "%B")

nee_by_date <- single_site_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         date_minus_year = format(date_object, '%m-%d')) %>% 
  group_by(date_minus_year) %>% 
  summarize(nee_mean = mean(NEE_VUT_REF), 
            nee_sd = sd(NEE_VUT_REF)) %>% 
  mutate(date_fake_year = ymd(paste0("2024-", date_minus_year)))

ggplot(nee_by_date, aes(x = date_fake_year, y = nee_mean)) +
  geom_ribbon(aes(ymax = nee_mean + nee_sd, ymin = nee_mean - nee_sd), 
              fill = "grey") + 
    geom_point() +
    labs(x = "Date",
         y = "Mean NEE +/- SD") +
    theme_minimal() + 
  scale_x_date(date_labels = "%B")


#' 
#' ### Average annual time series (multiple sites)
#' 
#' Textbook figures: 
#' 
#' ![](textbook_figures/seasonal_gpp_nee.png)
#' 
#' Show average daily values by site (symbols) and vegetation type (colors)
#' 
## -------------------------------------------------
#| code-fold: true
#| warning: false

gpp_by_date_sites <- multiple_sites_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), 
         date_minus_year = format(date_object, '%m-%d')) %>% 
  group_by(site, date_minus_year) %>% 
  summarize(gpp_mean = mean(GPP_NT_VUT_REF), 
            gpp_sd = sd(GPP_NT_VUT_REF), 
            reco_mean = mean(RECO_NT_VUT_REF), 
            reco_sd = sd(RECO_NT_VUT_REF), 
            nee_mean = mean(NEE_VUT_REF), 
            nee_sd = sd(NEE_VUT_REF)) %>% 
  mutate(date_fake_year = ymd(paste0("2024-", date_minus_year))) %>% 
  left_join(site_metadata, by = c("site" = "SITE_ID"))

ggplot(gpp_by_date_sites, aes(x = date_fake_year, y = gpp_mean)) +
  geom_point(aes(color = IGBP, shape = site)) +
  labs(x = "Date", 
       y = "Mean GPP") +
  theme_minimal() + 
  scale_x_date(date_labels = "%B") +
  scale_shape_manual(values = 1:length(unique(gpp_by_date_sites$site)))

ggplot(gpp_by_date_sites, aes(x = date_fake_year, y = reco_mean)) +
    geom_point(aes(color = IGBP, shape = site)) +
    labs(x = "Date",
         y = "Mean RECO") +
    theme_minimal() + 
  scale_x_date(date_labels = "%B") +
  scale_shape_manual(values = 1:length(unique(gpp_by_date_sites$site)))

ggplot(gpp_by_date_sites, aes(x = date_fake_year, y = nee_mean)) +
    geom_point(aes(color = IGBP, shape = site)) +
    labs(x = "Date",
         y = "Mean NEE") +
    theme_minimal() + 
  scale_x_date(date_labels = "%B") +
  scale_shape_manual(values = 1:length(unique(gpp_by_date_sites$site)))

#' ### Daily energy flux (single site)
#' 
#' Textbook figure: 
#' 
#' ![](textbook_figures/daily_energy_flux.png)
#' 
#' Energy flux variables: 
#' 
#' - NETRAD: Net radiation
#' - SW_IN_F: Shortwave radiation, incoming consolidated from SW_IN_F_MDS and SW_IN_ERA (negative values set to zero)
#' - SW_OUT: Shortwave radiation, outgoing
#' - LW_IN_F: Longwave radiation, incoming, consolidated from LW_IN_F_MDS and LW_IN_ERA
#' - LW_OUT: Longwave radiation, outgoing
#' 
#' Parsing dates and times: 
#' 
## -------------------------------------------------
rad_dt <- single_site_hourly %>% 
  mutate(date_object = ymd_hm(TIMESTAMP_START), 
         date = date(date_object), 
         time = format(as.POSIXct(date_object), format = '%H:%M')) 

#' 
#' Show average half-hourly shortwave, longwave, and total radiation for a single site. Data collection starts `{r} min(rad_dt$date)` and ends `{r} max(rad_dt$date)`. 
#' 
## -------------------------------------------------
#| code-fold: true

rad_means <- rad_dt %>% 
  group_by(time) %>% 
  summarise(rn_mean = mean(NETRAD), 
            sw_in_mean = mean(SW_IN_F), 
            sw_out_mean = mean(SW_OUT), 
            lw_in_mean = mean(LW_IN_F), 
            lw_out_mean = mean(LW_OUT)) %>% 
  pivot_longer(!time, names_to = "energy_flux_var", values_to = "energy_flux_value") %>% 
  mutate(energy_flux_var = factor(energy_flux_var, levels = c("rn_mean", "sw_in_mean", "sw_out_mean", "lw_in_mean", "lw_out_mean")))

ggplot(rad_means, aes(x = time, y = energy_flux_value)) +
  geom_line(aes(group = energy_flux_var, linetype = energy_flux_var)) +
  geom_hline(yintercept = 0) +
  #geom_point() +
  labs(x = "Time (hr)",
       y = "Energy flux (W m-2)") +
  theme_minimal() +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) +
  scale_linetype_manual(values = c("solid", "dotted", "dotted", "dashed", "dashed"))


#' 
#' ::: {.callout-tip}
#' ## Questions
#' 
#' 1. Why does this not really line up with the textbook image? 
#' 2. There are a few options for shortwave in besides SW_IN_F: SW_IN_POT, SW_IN_F_MDS, SW_IN_ERA. Which of these is the one we want? 
#' 3. Longwave in also has those options, plus JSB ones
#' 4. Add standard deviation to these means? 
#' :::
#' 
#' Textbook figure for APAR vs GPP for two sites: 
#' 
#' ![](textbook_figures/apar.png)
#' 
#' Show photosynthetically active radiation to GPP. 
#' 
#' ::: {.callout-tip}
#' # Question
#' 
#' **Which dataset is APAR in?** It's listed in the [data variables page](https://fluxnet.org/data/aboutdata/data-variables/), but not on the [fullset page](https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/) or in the single site hourly, daily, or annual datasets. Other **MET_RAD** variables like SW/LW radiation and photon flux density are in those datsets. Also not in BADM. 
#' :::
#' 
#' ### Comparison to meteorological variables (multiple sites)
#' 
#' Textbook figure (for NEE): 
#' 
#' ![](textbook_figures/meteo_comp.png)
#' 
#' Meteorological variables (bold indicates variable in figures): 
#' 
#' 1. P: Precipitation - not in annual datasets
#' 2. P_ERA: Precipitation, downscaled from ERA, linearly regressed using measured only site data
#' 3. **P_F**: Precipitation consolidated from P and P_ERA
#' 4. TA_F_MDS: Air temperature, gapfilled using MDS method
#' 5. TA_ERA: Air temperature, downscaled from ERA, linearly regressed using measured only site data
#' 6. **TA_F**: Air temperature, consolidated from TA_F_MDS and TA_ERA
#' 
#' Show annual precipitation, summed from daily data, against three main variables annually. 
#' 
## -------------------------------------------------
#| code-fold: true
#| warning: false

ggplot(multiple_sites_annual, aes(x = P_F, y = GPP_NT_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Precipitation (mm y-1)", y = "Annual GPP") +
  theme_minimal()

ggplot(multiple_sites_annual, aes(x = P_F, y = RECO_NT_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Precipitation (mm y-1)", y = "Annual RECO") +
  theme_minimal()

ggplot(multiple_sites_annual, aes(x = P_F, y = NEE_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Precipitation (mm y-1)", y = "Annual NEE") +
  theme_minimal()

#' 
#' Show annual temperature, averaged from daily data, against three main variables annually. 
#' 
## -------------------------------------------------
#| code-fold: true
#| warning: false

ggplot(multiple_sites_annual, aes(x = TA_F, y = GPP_NT_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Annual GPP") +
  theme_minimal()

ggplot(multiple_sites_annual, aes(x = TA_F, y = RECO_NT_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Annual RECO") +
  theme_minimal()

ggplot(multiple_sites_annual, aes(x = TA_F, y = NEE_VUT_REF, color = site)) +
  geom_point() +
  labs(x = "Temperature (C)", y = "Annual NEE") +
  theme_minimal()

#' 
#' ::: {.callout-tip}
#' ## Questions
#' 
#' 1. We want to do this for evapotranspiration; which variable is that in the dataset? 
#' 2. Do precipitation variable P and temp variable TA_F_MDS come from measured site data? 
#' 3. We could get some measure of variability for x- (temp and precip) and y- (GPP, RECO, NEE) axes by using the daily or monthly values instead; would that be of interest? 
#' :::
#' 
#' ### Interannual comparison to meteorological variables (multiple sites)
#' 
#' Textbook figure (for NPP): 
#' 
#' ![](textbook_figures/interannual_meteo_comp.png)
#' 
#' Organize data to get min and max annual values for two meteorological and three product variables. Then further reorganize data to get all plots into single figure. 
#' 
## -------------------------------------------------
interannual_met <- multiple_sites_annual %>% 
  group_by(site) %>% 
  summarize(min_precip = min(P_F), 
            max_precip = max(P_F), 
            min_temp = min(TA_F), 
            max_temp = max(TA_F), 
            min_gpp = min(GPP_NT_VUT_REF), 
            max_gpp = max(GPP_NT_VUT_REF), 
            min_reco = min(RECO_NT_VUT_REF), 
            max_reco = max(RECO_NT_VUT_REF), 
            min_nee = min(NEE_VUT_REF), 
            max_nee = max(NEE_VUT_REF)) %>% 
  pivot_longer(cols = -site, names_to = c("min_or_max", ".value"), names_sep = "_")

interannual_met_pairs <- interannual_met %>% 
  mutate(precip2 = precip, 
         precip3 = precip, 
         temp2 = temp, 
         temp3 = temp, 
         gpp2 = gpp, 
         reco2 = reco, 
         nee2 = nee) %>% 
  rename(precipgpp_mvvalue = precip, precipgpp_pvvalue = gpp, 
         preciprec_mvvalue = precip2, preciprec_pvvalue = reco, 
         precipnee_mvvalue = precip3, precipnee_pvvalue = nee, 
         tempergpp_mvvalue = temp, tempergpp_pvvalue = gpp2, 
         temperrec_mvvalue = temp2, temperrec_pvvalue = reco2, 
         tempernee_mvvalue = temp3, tempernee_pvvalue = nee2) %>% 
  pivot_longer(!c(site, min_or_max), names_to = c("pairs", ".value"), 
               names_sep = "_") %>% 
  separate_wider_position(pairs, widths = c(met_var = 6, pro_var = 3)) %>% 
  rename(mv_value = mvvalue, pv_value = pvvalue) %>% 
  mutate(met_var = as.factor(met_var), 
         met_label = factor(met_var, labels = c("Precipitation (mm y-1)", 
                                                "Temperature (C)")), 
         pro_var = factor(pro_var, labels = c("gpp", "rec", "nee")), 
         pro_label = factor(pro_var, labels = c("Annual GPP", 
                                                "Annual RECO", 
                                                "Annual NEE")))

#' 
#' Show minimum and maximum interannual precipitation/temperature compared to min and max interannual GPP/RECO/NEE. 
#' 
## -------------------------------------------------
#| code-fold: true
#| warning: false

ggplot(interannual_met_pairs, aes(x = mv_value, y = pv_value, group = site, color = site)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(pro_label), cols = vars(met_label), scales = "free") +
  labs(x = "", y = "")

#' 
#' ::: {.callout-tip}
#' ## Question
#' 
#' 1. These figures match up the lowest annual meteorological variable with the lowest product variable for each site, with the highest. Is that what the textbook figure is displaying? 
#' 2. There is no trend line because there doesn't really seem to be a trend here. One might emerge once we add more sites? 
#' :::
#' 
#' ### Bowen ratios
#' 
#' Textbook figure: 
#' 
#' ![](textbook_figures/bowen_ratios.png)
#' 
#' Heat flux variables (bold indicates variable in figures): 
#' 
#' - **H_F_MDS**: Sensible heat flux, gapfilled using MDS method (W m-2)
#' - H_CORR (H_CORR_25, H_CORR_75): Sensible heat flux, corrected H_F_MDS by energy balance closure correction factor (25th and 75th percentile) 
#' - **LE_F_MDS**: Latent heat flux, gapfilled using MDS method
#' - LE_CORR (LE_CORR_25, LE_CORR_75): Latent heat flux, corrected LE_F_MDS by energy balance closure correction factor (25th and 75th percentile) 
#' 
#' 
#' Visual check of heat flux variable ranges. 
## -------------------------------------------------
#| message: false
ggplot(multiple_sites_daily, aes(x = LE_F_MDS)) +
  geom_histogram() +
  facet_wrap(~site)

ggplot(multiple_sites_daily, aes(x = H_F_MDS)) +
  geom_histogram() +
  facet_wrap(~site)

#' 
#' Show average daily summer-only sensible vs latent heat flux (i.e., Bowen ratio) for multiple sites with different vegetation types. Bowen ratios of 3, 2, 1, 0.5, and 0.25 shown by dotted lines. 
#' 
## -------------------------------------------------
#| code-fold: true

multiple_sites_flux <- multiple_sites_daily %>% 
  mutate(date_object = ymd(TIMESTAMP), .before = 1, 
         month = format(date_object, '%m')) %>% 
  filter(month %in% c("06", "07", "08")) %>% 
  group_by(site) %>% 
  summarize(latent_heat_flux = mean(LE_F_MDS), 
            sensible_heat_flux = mean(H_F_MDS)) %>% 
  left_join(site_metadata, by = c("site" = "SITE_ID"))

upper_axis_limit <- pmax(max(multiple_sites_flux$sensible_heat_flux), max(multiple_sites_flux$latent_heat_flux)) + 10

ggplot(multiple_sites_flux, aes(x = latent_heat_flux, y = sensible_heat_flux, color = IGBP)) +
  geom_point(size = 3) +
  geom_abline(slope = 0.25, linetype = "dotted", color = "darkgrey") +
  geom_abline(slope = 0.5, linetype = "dotted", color = "darkgrey") +
  geom_abline(slope = 1, linetype = "dotted", color = "darkgrey") +
  geom_abline(slope = 2, linetype = "dotted", color = "darkgrey") +
  geom_abline(slope = 3, linetype = "dotted", color = "darkgrey") +
  labs(x = "Latent Heat Flux (W m-2)", y = "Sensible Heat Flux (W m-2)") +
  theme_classic() +
  guides(x.sec = "axis", y.sec = "axis") +
  theme(axis.ticks.x.top = element_blank(), 
        axis.text.x.top = element_blank(), 
        axis.ticks.y.right = element_blank(), 
        axis.text.y.right = element_blank()) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, upper_axis_limit)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, upper_axis_limit))



#' 
#' ::: {.callout-tip}
#' ## Questions
#' 
#' 1. Should watts be converted to megajoules for the heat flux variables? 
#' 2. Are heat flux values expected to be negative, especially for sensible heat flux? 
#' :::
#' 
#' ### Biome plot
#' 
#' Textbook figure: 
#' 
#' ![](textbook_figures/biomes.png)
#' 
#' The R package [`plotbiomes`](https://github.com/valentinitnelav/plotbiomes) can be used to recreate the base plot here, which is referred to as a [Whittaker plot](https://en.wikipedia.org/wiki/Biome#Whittaker_(1962,_1970,_1975)_biome-types). It includes a dataset of temperature, precipitation, and associated biome. 
#' 
## -------------------------------------------------
data("Whittaker_biomes")
head(Whittaker_biomes)
unique(Whittaker_biomes$biome)

#' 
#' Get mean values of annual precipitation (P_F) and temperature (TA_F) for each site, and convert precipitation to match Whittaker dataset units (mm/yr to cm/yr). 
#' 
## -------------------------------------------------
whittaker_format <- multiple_sites_annual %>% 
  group_by(site) %>% 
  summarize(mean_precip = mean(P_F), 
            mean_temp = mean(TA_F)) %>% 
  mutate(mean_precip_cm = conv_unit(mean_precip, "mm", "cm")) %>% 
  left_join(site_metadata, by = c("site" = "SITE_ID"))

#' 
#' Show mean annual precipitation and temperature of each site over Whittaker biome. 
#' 
## -------------------------------------------------
#| code-fold: true

ggplot() +
  geom_polygon(data = Whittaker_biomes, aes(x = temp_c, y = precp_cm, color = biome), fill = "white") +
  scale_color_brewer(palette = "BrBG") +
  new_scale_color() +
  geom_point(data = whittaker_format, aes(x = mean_temp, y = mean_precip_cm, color = IGBP)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Temperature (C)", y = "Precipitation (cm y-1)") +
  theme_classic()

#' ::: {.callout-tip}
#' ## Questions
#' 
#' 1. What to do with the plot colors? We could match IGBP colors to biome colors. Or make the biome colors represent their type, e.g., tan for desert, green for rain forest. Or turn the biome types into labels like the original figure. Lots of options here...
#' :::
