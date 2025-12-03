#' Detect start and end of season from daily GPP data
#' 
#' An implementation of the method described by Panwar et al. (2023) to detect
#' the start and end of season using smoothed, cumulative GPP data.
#' 
#' @author Eric R. Scott
#' @references 
#' Panwar, A., Migliavacca, M., Nelson, J.A., Cortés, J., Bastos, A., Forkel,
#' M., Winkler, A.J., 2023. Methodological challenges and new perspectives of
#' shifting vegetation phenology in eddy covariance data. Sci Rep 13, 13885.
#' https://doi.org/10.1038/s41598-023-41048-x
#' 
#' @param daily Daily flux data including at least a `site` column, a column for
#'   date (specified by `date_var`) and a column for GPP or another flux
#'   (specified by `flux_var`).
#' @param knots Number of knots to use. Passed to the `df` argument of
#'   `smooth.spline()`
#' @param date_var Character; the name of the column containing the date.
#' @param flux_var Character; the name of the column containing the flux data.
#' @param method Currently only `"threshold"` is implemented.
#' @param threshold The threshold to use for detecting the start and end of
#'   season
#' @param keep_data Logical; if TRUE, the output contains a list column with the
#'   raw and fitted data that can be used, e.g., for plotting the smoothed fit for
#'   each site.

# TODO: fix southern hemisphere sites.  The way the data is split by year needs
# to be handled differently so the "center" of the year is offset by 6 months.
detect_phenology_integral <- function(
  daily,
  knots = 10,
  date_var = "date",
  flux_var = "GPP_NT_VUT_MEAN",
  method = c("threshold", "derivative"),
  threshold = 0.2,
  keep_data = FALSE
) {
  daily %>%
    group_split(site) %>%
    map(\(df) {
      detect_phenology_integral_site(
        df,
        knots = knots,
        date_var = date_var,
        flux_var = flux_var,
        keep_data = keep_data
      )
    }) %>%
    list_rbind()
}

#' Just for one site
detect_phenology_integral_site <- function(
  daily_site,
  knots = 10,
  date_var = "date",
  flux_var = "GPP_NT_VUT_MEAN",
  method = c("threshold", "derivative"),
  threshold = 0.2,
  keep_data = FALSE
) {
  daily_site <- daily_site %>%
    filter(!is.na(.data[[flux_var]])) %>%
    mutate(
      year = lubridate::year(.data[[date_var]]),
      DOY = lubridate::yday(.data[[date_var]]),
      hemisphere = if_else(LOCATION_LAT < 0, "SH", "NH"),
      climate_zone = case_when(
        abs(LOCATION_LAT) < 23.5 ~ "Tropical",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT >= 0 ~ "Temperate_North",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT < 0 ~ "Temperate_South",
        .default = "Other"
      )
    )
  if (nrow(daily_site) == 0) {
    return(tibble())
  }
  years <-
    unique(daily_site$year) %>%
    sort()

  smooths <- map(years, \(focal_year) {
    start <- make_date(focal_year) - days(30)
    end <- ceiling_date(make_date(focal_year), "year") + days(29)

    # TODO need to actually split the data differently in the southern 
    # hemisphere so the "center" is 6-months offset
    padded_data <- daily_site %>%
      filter(between(date, start, end)) %>%
      arrange(date) %>%
      mutate(year = focal_year) %>%
      mutate(
        DOY_padded = case_when(
          year(date) == focal_year - 1 ~ (date - make_date(focal_year)) %>%
            as.numeric("days"),
          year(date) == focal_year + 1 ~ DOY + 365 + leap_year(focal_year),
          .default = DOY
        )
      ) %>%
      # TODO do we still need this?  I think this is just for plotting southern
      # hemisphere and northern hemisphere sites on the same x-axis, but I 
      # think I'd rather let the user do this outside of this function.
      mutate(
        DOY_aligned = case_when(
          climate_zone == "Temperate_South" ~ (DOY_padded + 182) %% 365,
          .default = DOY_padded
        )
      ) %>%
      mutate(cumflux = cumsum(GPP_NT_VUT_MEAN))

    m <- smooth.spline(
      x = 1:nrow(padded_data),
      y = padded_data$cumflux,
      df = knots
    )

    bind_cols(padded_data, smooth = predict(m, deriv = 1)$y)
  }) %>%
    set_names(years)

  out <- smooths %>%
    map(\(df) {
      max_flux <- max(df$smooth, na.rm = TRUE)
      df %>%
        summarize(
          SOS = DOY_padded[which.max(smooth >= max_flux * threshold)],
          POS = DOY_padded[smooth == max_flux],
          EOS = DOY_padded[which.max(
            smooth <= max_flux * threshold & DOY_padded > POS
          )],
          climate_zone = unique(climate_zone),
          site = unique(site)
        ) %>%
        mutate(
          issues = case_when(
            is.na(SOS) | is.na(POS) | is.na(EOS) ~ "NA for SOS, POS, or EOS",
            SOS == 1 ~ "SOS == 1 (possibly spurrious)",
            climate_zone != "Tropical" &&
              (SOS >= POS |
                POS >= EOS) ~ "Order violation (SOS >= POS or POS >= EOS)"
          )
        )
    }) %>%
    list_rbind(names_to = "year") %>%
    mutate(year = as.integer(year)) %>%
    select(site, year, SOS, POS, EOS, issues)

  # optionaly keep the original data as a list column for plotting
  if (isTRUE(keep_data)) {
    smooths_min <- smooths %>%
      map(\(df) {
        df %>% select(date, all_of(flux_var), DOY, DOY_padded, DOY_aligned, smooth)
      })
    out$data <- smooths_min
  }

  out
}