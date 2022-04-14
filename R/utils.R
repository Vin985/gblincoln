#' @title Update a list with another one
#' @description This function takes two lists with named elements, the one to
#' be updated and the #' one with the updates. If elements from the new list are
#' not present in the one to be updated, they will be added. If they are presents,
#' their value will be changed. Note that if elements are not named, they will
#' not be updated.
#' @param old A list to be updated
#' @param new A list with values to be added or updated
#' @param new_first Should the values found in the new list not present in the
#' old list be added at the beginning of the list or the end, Default: FALSE
#' @return The updated list
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
