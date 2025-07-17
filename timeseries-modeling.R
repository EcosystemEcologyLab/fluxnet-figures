# Load data
library(dplyr)
source("fcn_utility_FLUXNET.R")
config <- list(
  daily_cache = "data/multiple_sites_daily.rds",
  annual_cache = "data/multiple_sites_annual.rds",
  columns_to_clean = c("GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "RECO_NT_VUT_REF", "NEE_VUT_REF", "LE_F_MDS", "H_F_MDS", "PPFD_IN", "TA_F_MDS")
)
site_metadata <- load_fluxnet_metadata()
daily <- load_and_clean_daily_data(site_metadata = site_metadata)


# Convert to tsibble
library(tsibble)

daily_ts <- daily |> 
  select(date_object, site, IGBP, NEE_VUT_MEAN) |> 
  as_tsibble(key = c(site, IGBP), index = date_object)
  

# Timeseries decomposition  
library(feasts)
library(fable)
daily_ts |> filter(site == first(site)) |> gg_season()
#something obviously wrong with 2020 data

daily |> 
  filter(site == "BE-Bra") |> 
  ggplot(aes(NEE_VUT_MEAN)) +
  geom_histogram() + facet_wrap(~year(date_object))
#hmmm, guess I'll try different sites

sites <- daily_ts |>
  group_by(site) |> 
  #sites with at least 5 years of data
  filter(any(date_object < "2015-01-01") & any(date_object > "2020-01-01")) |> 
  pull(site) |>
  unique()
set.seed(345089)
rand_sites <- sample(sites, 5)

daily_ts |>  
  filter(site %in% rand_sites) |>
  gg_season()
fit <- 
  daily_ts |>
  group_by(IGBP) |> 
  filter(site %in% rand_sites) |> 
  model(
    stl = STL(NEE_VUT_MEAN ~ trend() + season(period = "year")),
    ets = ETS(NEE_VUT_MEAN ~ trend(method = c("N", "A", "Ad")) + season()),
    tslm = TSLM(NEE_VUT_MEAN ~ trend() + season()),
    arima = ARIMA(NEE_VUT_MEAN) #super slow, specify fully after auto-fit maybe
  )
fit

fit |>
  select(stl) |> 
  components() |>  
  autoplot()

fit |>
  select(ets) |> 
  components() |> 
  autoplot()

fit |> 
  select(tslm) |> 
  glance()

fit |> 
  select(arima) |> 
  glance()

fit |> 
  accuracy() |> 
  arrange(site, RMSE)
#ARIMA does consistently best, but doesn't do timeseries decomposition
#ETS does timeseries decomp, but generally shows no significant trend

# Is there a way to treat sites as replicates within a IGBP category with this package?