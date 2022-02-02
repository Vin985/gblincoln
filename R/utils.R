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
update_list <- function(old, new){
  tmp = old
  if (!is.null(new)){
    for (i in seq_along(new)){
      var = names(new)[i]
      tmp[var] = new[i]
    }
  }
  return(tmp)
}
