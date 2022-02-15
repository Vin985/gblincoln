


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
  by_groups <- (
    if (by_band) {
      c("b.year", "add_info")
    } else {
      c("age_short", "b.year", "r.corrected_year")
    }
  )
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
get_all_direct_recoveries <- function(banding_filters, recoveries_filters) {
  banding_db <- lincoln_filter_db(banding_filters)
  bandings <- get_banding_summary(banding_db)

  rec_db <- lincoln_filter_db(recoveries_filters, type="recoveries")
  recoveries <- get_recoveries_summary(rec_db)

  direct_recoveries <- merge(bandings, recoveries)
  return(direct_recoveries)
}
