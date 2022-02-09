

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
#' @rdname summarize_banding
#' @export
summarize_banding <- function(df) {
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
#' @rdname summarize_recoveries
#' @export
summarize_recoveries <- function(df) {
  res = df %>%
    group_by(r.month) %>%
    summarise(total = n())
  return(res)
}
