#' Detect start and end of season from daily GPP data
#'
#' An implementation of the method described by Panwar et al. (2023) to detect
#' the start and end of season using smoothed, cumulative GPP data. This method
#' uses a cumulative-sum-based smoothing technique. The data is padded with ±30
#' days on either end of each year, spline-smoothed, and differentiated to
#' obtain a smooth GPP curve. SOS and EOS are based on 20% of the max smoothed
#' value, and POS is the day of peak. This approach increases signal-to-noise
#' ratio and is more robust to parameter choices than direct smoothing.
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
#' @param method Currently only `"threshold"` is implemented which defines SOS
#'   and EOS as a threshold of the max smoothed value.
#' @param threshold The threshold to use for detecting the start and end of
#'   season
#' @param keep_data Logical; if TRUE, the output contains a list column with the
#'   raw and fitted data that can be used, e.g., for plotting the smoothed fit for
#'   each site.
#' @returns A tibble with columns for start of season (`SOS`), peak of season
#'   (`POS`), and end of season (`EOS`) as well as a column, `issues`, with
#'   descriptions of any problems (e.g. EOS is before SOS). If `keep_data` is
#'   set to `TRUE`, then there is also a list-column, `data`, that contains
#'   date, the `flux_var`, and the fitted smooth which can be useful for
#'   plotting.
#' @seealso [tidyr::unnest()] for dealing with list-columns.
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
    dplyr::group_split(site) %>%
    purrr::map(\(df) {
      detect_phenology_integral_site(
        df,
        knots = knots,
        date_var = date_var,
        flux_var = flux_var,
        method = method,
        threshold = threshold,
        keep_data = keep_data
      )
    }) %>%
    purrr::list_rbind()
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
  method <- match.arg(method)
  if (method == 'derivative') {
    stop(
      "The derivative method is not implemented yet!  Please use `method = 'threshold'`."
    )
  }
  # If there are duplicated dates, the fitted values from the spline won't match
  # up with the data
  if(anyDuplicated(daily_site[[date_var]]) > 0) {
    cli::cli_abort("Duplicate dates detected in site {unique(daily_site$site)}")
  }
  daily_site <- daily_site %>%
    dplyr::filter(!is.na(.data[[flux_var]])) %>%
    dplyr::mutate(
      year = lubridate::year(.data[[date_var]]),
      DOY = lubridate::yday(.data[[date_var]]),
      hemisphere = dplyr::if_else(LOCATION_LAT < 0, "SH", "NH"),
      climate_zone = dplyr::case_when(
        abs(LOCATION_LAT) < 23.5 ~ "Tropical",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT >= 0 ~ "Temperate_North",
        abs(LOCATION_LAT) >= 23.5 & LOCATION_LAT < 0 ~ "Temperate_South",
        .default = "Other"
      )
    )
  
  # If there's no flux data for a site, no point in going further
  if (nrow(daily_site) == 0) {
    return(dplyr::tibble())
  }

  years <-
    unique(daily_site$year) %>%
    sort()
  
  # Fit smooth to cumulative flux
  smooths <- purrr::map(years, \(focal_year) {
    hemisphere <- unique(daily_site$hemisphere)
    if (hemisphere == "NH") {
      start <- lubridate::make_date(focal_year) - lubridate::days(30)
      end <- lubridate::ceiling_date(lubridate::make_date(focal_year), "year") +
        lubridate::days(29)
      # Shift center of period 6 months forward if in southern hemisphere
    } else {
      start <- lubridate::make_date(focal_year) -
        lubridate::days(30) +
        months(6)
      end <- lubridate::ceiling_date(lubridate::make_date(focal_year), "year") +
        lubridate::days(29) +
        months(6)
    }
    
    # For each "focal year" subset data such that it includes dates from the
    # previous and next years
    padded_data <- daily_site %>%
      dplyr::filter(dplyr::between(.data[[date_var]], start, end)) %>%
      dplyr::arrange(.data[[date_var]]) %>%
      dplyr::mutate(year = focal_year) %>%
      dplyr::mutate(
        DOY_padded = dplyr::case_when(
          lubridate::year(.data[[date_var]]) == focal_year - 1 ~ (.data[[date_var]] -
            lubridate::make_date(focal_year)) %>%
            as.numeric("days"),
          lubridate::year(.data[[date_var]]) == focal_year + 1 ~ DOY +
            365 +
            lubridate::leap_year(focal_year),
          .default = DOY
        )
      ) %>%
      dplyr::mutate(cumflux = cumsum(GPP_NT_VUT_MEAN))

    m <- smooth.spline(
      x = padded_data$DOY_padded, 
      y = padded_data$cumflux,
      df = knots
    )

    dplyr::bind_cols(padded_data, smooth = predict(m, deriv = 1)$y)
  }) %>%
    purrr::set_names(years)

  out <- smooths %>%
    purrr::map(\(df) {
      max_flux <- max(df$smooth, na.rm = TRUE)
      df %>%
        dplyr::summarize(
          SOS = DOY_padded[which.max(smooth >= max_flux * threshold)],
          POS = DOY_padded[smooth == max_flux],
          EOS = DOY_padded[which.max(
            smooth <= max_flux * threshold & DOY_padded > POS
          )],
          climate_zone = unique(climate_zone),
          site = unique(site)
        ) %>%
        dplyr::mutate(
          issues = dplyr::case_when(
            is.na(SOS) | is.na(POS) | is.na(EOS) ~ "NA for SOS, POS, or EOS",
            SOS == 1 ~ "SOS == 1 (possibly spurrious)",
            climate_zone != "Tropical" &&
              (SOS >= POS |
                POS >= EOS) ~ "Order violation (SOS >= POS or POS >= EOS)"
          )
        )
    }) %>%
    purrr::list_rbind(names_to = "year") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::select(site, year, SOS, POS, EOS, issues)

  # optionaly keep the original data as a list column for plotting
  if (isTRUE(keep_data)) {
    smooths_min <- smooths %>%
      purrr::map(\(df) {
        df %>%
          dplyr::select(
            dplyr::all_of(c(date_var, flux_var)),
            DOY,
            DOY_padded,
            smooth
          )
      })
    out$data <- smooths_min
  }

  out
}