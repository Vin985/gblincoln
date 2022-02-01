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
#' @rdname update_list
#' @export
update_list <- function(new, old){
  if (is.null(new)){
    return(old)
  } else {
    tmp = old
    for (i in seq_along(new)){
      var = names(new)[i]
      if (var %in% names(tmp)) {
        tmp[var] = new[i]
      }
    }
    return(tmp)
  }
}
