









#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_banding_summary
#' @export
get_banding_summary <- function(df) {
  res = df %>%
    group_by(b.year) %>%
    summarise(total = sum(count_of_birds))
  return(res)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_recoveries_summary
#' @export
get_recoveries_summary <- function(df, by_band = FALSE) {
  by_groups <- (if (by_band) {
    c("b.year", "add_info")
  } else {
    c("age_short", "b.year", "r.corrected_year")
  })
  res <- df %>% filter(b.year == r.corrected_year) %>%
    group_by_at(by_groups) %>%
    summarise(total_recoveries = n())
  return(res)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_all_direct_recoveries
#' @export
get_all_direct_recoveries <-
  function(banding_filters,
           recoveries_filters,
           band_type = "all",
           banding_df = NULL,
           recoveries_df = NULL) {
    banding_db <-
      lincoln_filter_db(banding_filters,
                        df = banding_df,
                        type = "banding",
                        band_type = band_type)
    bandings <- get_banding_summary(banding_db)

    rec_db <-
      lincoln_filter_db(
        recoveries_filters,
        df = recoveries_df,
        type = "recoveries",
        band_type = band_type
      )
    recoveries <- get_recoveries_summary(rec_db)

    direct_recoveries <-
      merge(bandings, recoveries) %>% mutate(drr = total_recoveries /
                                               total)
    return(direct_recoveries)
  }

#' @rdname get_hr_df
#' @export
get_hr_df <-
  function(drr_df = NULL,
           rho_df = NULL,
           band_type = "all",
           ...) {
    if (is.null(drr_df)) {
      drr_df <- get_all_direct_recoveries(band_type = band_type, ...)
    }
    if (is.null(rho_df)) {
      rho_df = REPORTING_PROBABILITIES
    }
    hr_df <- inner_join(drr_df, rho_df, by = "b.year")
    hr_df <- hr_df %>%
      mutate(hr = drr / rho) %>%
      mutate(var_drr = (drr * (1 - drr)) / (total - 1)) %>%
      mutate(se_drr = sqrt(var_drr)) %>%
      mutate(var_h = (var_drr / (rho ^ 2)) + ((drr ^ 2 * var_rho) / rho ^ 4)) %>%
      mutate(se_h = sqrt(var_h)) %>%
      mutate(cl_h = se_h * 1.96) %>%
      mutate(cv = se_h / hr) %>%
      mutate(band_type = band_type) %>%
      select(-age_short)
    return(hr_df)
  }



get_confidence_levels <- function(df){
  return(as.data.frame(cbind(
    b.year = df$b.year,
    start = (df$h - df$cl_h),
    end = (df$h + df$cl_h)
  )))
}

#' @rdname check_all_bands
#' @export
compare_band_types <-
  function(hr_all = NULL,
           hr_no_geo = NULL,
           plot = TRUE,
           check_overlap = TRUE,
           geo_years=GEO_YEARS,
           ...) {
    # All Bands
    if (is.null(hr_all)) {
      hr_all <- get_hr_df(..., band_type = "all")
    }
    # No geolocator
    if (is.null(hr_no_geo)) {
      hr_no_geo <- get_hr_df(band_type = "no_geo", ...)
    }


    no_geo_subset <- hr_no_geo[hr_no_geo$b.year %in% geo_years, ]

    if (plot) {
      library(ggplot2)
      hr_by_band_type <- rbind(hr_all_bands, no_geo_subset)
      both_plot <-
        ggplot(data = hr_by_band_type, aes(x = b.year, y = hr)) +
        geom_point(aes(color = band_type)) +
        geom_errorbar(aes(ymin = hr - cl_h, ymax = hr + cl_h))
      both_plot
    }

    if (check_overlap){
      ## Compute overlap
      # Get only relevant year for all bands
      all_subset <-
        hr_all[hr_all$b.year %in% geo_years, ]
      # Get confidence levels for all bands
      all_cl <- get_confidence_levels(all_subset)
      # Get confidence levels without geolocators
      no_geo_cl <-get_confidence_levels(no_geo_subset)
      # Check if there is an overlap
      overlap <-
        (no_geo_cl$start <= all_cl$end) & (no_geo_cl$end >= all_cl$start)
      # Return TRUE if all confidence levels overlap
      res <-
        list(
          overlap = all(overlap),
          band_type = ifelse(overlap, "all", "no_geo"),
          hr_df = ifelse(overlap, hr_all, hr_no_geo)
        )
      return(res)
    }
  }
