#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param new PARAM_DESCRIPTION
#' @param old PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list_update
#' @export
list_update_bak <- function(old, new, new_first=FALSE){
  tmp = old
  if (!is.null(new)){
    for (i in seq_along(new)){
      var = names(new)[i]
      tmp[var] = new[i]
    }
  }
  return(tmp)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param new PARAM_DESCRIPTION
#' @param old PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname list_update
#' @export
list_update <- function(old, new, new_first=FALSE){
  res = old
  in_both = names(new)[names(new) %in% names(old)]
  res[in_both] = new[in_both]
  if (new_first){
    res = c(new[!names(new) %in% in_both], res)
  } else {
    res = c(res, new[!names(new) %in% in_both])
  }
  return(res)
}
