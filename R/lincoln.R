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
    summarise(total_banding = sum(count_of_birds))
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
  function(filters,
           band_type = "all",
           banding_df = NULL,
           recoveries_df = NULL) {
    banding_db <-
      lincoln_filter_db(filters,
                        df = banding_df,
                        type = "banding",
                        band_type = band_type)
    bandings <- get_banding_summary(banding_db)

    rec_db <-
      lincoln_filter_db(filters,
                        df = recoveries_df,
                        type = "recoveries",
                        band_type = band_type)
    recoveries <- get_recoveries_summary(rec_db)

    direct_recoveries <-
      merge(bandings, recoveries) %>% mutate(drr = total_recoveries /
                                               total_banding)
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
      mutate(var_drr = (drr * (1 - drr)) / (total_banding - 1)) %>%
      mutate(se_drr = sqrt(var_drr)) %>%
      mutate(var_h = (var_drr / (rho ^ 2)) + ((drr ^ 2 * var_rho) / rho ^ 4)) %>%
      mutate(se_h = sqrt(var_h)) %>%
      mutate(cl_h = se_h * 1.96) %>%
      mutate(cv = se_h / hr) %>%
      mutate(band_type = band_type) %>%
      select(-age_short)
    return(hr_df)
  }



get_confidence_levels <- function(df) {
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
           geo_years = GEO_YEARS,
           ...) {
    # All Bands
    if (is.null(hr_all)) {
      hr_all <- get_hr_df(..., band_type = "all")
    }
    # No geolocator
    if (is.null(hr_no_geo)) {
      hr_no_geo <- get_hr_df(band_type = "no_geo", ...)
    }


    no_geo_subset <- hr_no_geo[hr_no_geo$b.year %in% geo_years,]

    if (plot) {
      library(ggplot2)
      hr_by_band_type <- rbind(hr_all_bands, no_geo_subset)
      both_plot <-
        ggplot(data = hr_by_band_type, aes(x = b.year, y = hr)) +
        geom_point(aes(color = band_type)) +
        geom_errorbar(aes(ymin = hr - cl_h, ymax = hr + cl_h))
      print(both_plot)
    }

    if (check_overlap) {
      ## Compute overlap
      # Get only relevant year for all bands
      all_subset <-
        hr_all[hr_all$b.year %in% geo_years,]
      # Get confidence levels for all bands
      all_cl <- get_confidence_levels(all_subset)
      # Get confidence levels without geolocators
      no_geo_cl <- get_confidence_levels(no_geo_subset)
      # Check if there is an overlap
      overlap <-
        (no_geo_cl$start <= all_cl$end) &
        (no_geo_cl$end >= all_cl$start)
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


#' @rdname get_lincoln_estimates
#' @export
get_lincoln_estimates <-
  function(filters,
           hr_df = NULL,
           harvest_df = NULL,
           plot_estimates = TRUE,
           save_estimates = TRUE,
           save_path = ".") {
    if (is.null(hr_df)) {
      # TODO: how to select best hr_df? Compare bands?
      hr_df <- get_hr_df(filters=filters)
    }
    if (is.null(harvest_df)) {
      # Should be an error since it is a big portion of estimates
      # TODO
      stop("A harvest dataframe for the selected species should be provided")
    }
    lincoln_df <- inner_join(hr_df, harvest_df, by = "b.year")
    lincoln <- lincoln_df %>%
      mutate(harvest_adj = harvest * 0.61) %>%
      mutate(se_harvest_adj = se_harvest * 0.61) %>%
      mutate(var_harvest_adj = se_harvest_adj ^ 2) %>%
      mutate(N = ((((total_banding + 1) * (harvest_adj + 1) * rho
      ) / (
        total_recoveries + 1
      )) - 1)) %>%
      mutate(var_N_b.r = ((total_banding + 1) * (total_banding - total_recoveries)) / (((
        total_recoveries +
          1
      ) ^ 2) * (total_recoveries + 2))) %>%
      mutate(var_N_bH.r =  ((total_banding / total_recoveries) ^ 2) * var_harvest_adj + harvest_adj ^
               2 * var_N_b.r) %>%
      mutate(var_N = (((total_banding * harvest_adj) / total_recoveries
      ) ^ 2) * var_rho + rho ^
        2 * var_N_bH.r) %>%
      mutate(se_N = sqrt(var_N)) %>%
      mutate(cl_N = se_N * 1.96)


    if (plot_estimates) {
      library(ggplot2)
      plt <- ggplot(data = lincoln, aes(x = b.year, y = N)) +
        geom_point() + geom_smooth() +
        scale_y_continuous(name = "Abundance (Lincoln)", limits = c(0, 500000)) +
        geom_errorbar(aes(ymin = N - cl_N, ymax = N + cl_N)) +
        labs(x = "Year")
      print(plt)
    }

    lincoln_est <-
      as.data.frame(cbind(lincoln$b.year, lincoln$N, lincoln$cl_N))
    colnames(lincoln_est) <- c("year", "N", "NCL")
    if (save_estimates) {
      file_path = file.path(save_path,
                            sprintf(
                              '%s_Lincoln_2000_2019_%s.csv',
                              filters$SPEC,
                              format(Sys.time(), "%b%d_%Y")
                            ))
      write.csv(lincoln_est,
                file_path)
    }
    return(lincoln_est)

  }

#' @rdname lincoln_estimates
#' @export
lincoln_estimates <- function(filters_list, ...) {
  res = list()
  if (length(filters_list) == 0) {
    print("At least one set of filters must be defined")
  } else {
    for (filter in filters_list) {
      sprintf("Calculating Lincoln estimates for filter %s",
              toString(lapply(1:length(filters), function(i, filters) {
                return(paste(names(filters)[i], as.character(filters[i]), sep = ": "))
              }, filters)))
      # TODO: make the function more user friendly
      estimates <- get_lincoln_estimates(filter, ...)
      res <- c(res, estimates)
    }
  }
  return(res)
}
