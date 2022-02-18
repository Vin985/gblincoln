#' @title Gamebirds Banding Dataset
#' @description DATASET_DESCRIPTION
#' @format A data frame with 16068 rows and 43 variables:
#' \describe{
#'   \item{\code{add_info}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{age_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{coordinate_precision}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{day_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{dir}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{flyway_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{lat}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{long}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{month_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{region_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{year}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{band_prefix_plus}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{band_size}}{character COLUMN_DESCRIPTION}
#'   \item{\code{band_type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{count_of_birds}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{country_code}}{character COLUMN_DESCRIPTION}
#'   \item{\code{GISBLat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{GISBLong}}{double COLUMN_DESCRIPTION}
#'   \item{\code{how_aged}}{character COLUMN_DESCRIPTION}
#'   \item{\code{how_sexed}}{character COLUMN_DESCRIPTION}
#'   \item{\code{permit}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sex_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{sp_num}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{state_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{status_code}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{age}}{character COLUMN_DESCRIPTION}
#'   \item{\code{VAI}}{character COLUMN_DESCRIPTION}
#'   \item{\code{btype_vbtype}}{character COLUMN_DESCRIPTION}
#'   \item{\code{btype_text}}{character COLUMN_DESCRIPTION}
#'   \item{\code{location_accuracy_desc}}{character COLUMN_DESCRIPTION}
#'   \item{\code{day_span}}{character COLUMN_DESCRIPTION}
#'   \item{\code{how_aged_desc}}{character COLUMN_DESCRIPTION}
#'   \item{\code{how_sexed_desc}}{character COLUMN_DESCRIPTION}
#'   \item{\code{country_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{state_name}}{character COLUMN_DESCRIPTION}
#'   \item{\code{month}}{character COLUMN_DESCRIPTION}
#'   \item{\code{permittee}}{character COLUMN_DESCRIPTION}
#'   \item{\code{flyway}}{character COLUMN_DESCRIPTION}
#'   \item{\code{state}}{character COLUMN_DESCRIPTION}
#'   \item{\code{sex}}{character COLUMN_DESCRIPTION}
#'   \item{\code{SPEC}}{character COLUMN_DESCRIPTION}
#'   \item{\code{species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{status}}{character COLUMN_DESCRIPTION}
#'   \item{\code{age_short}}{character COLUMN_DESCRIPTION}
#'}
"gb_banding"


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param col_names PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname check_columns
#' @export
check_columns <- function(col_names) {
  old_cols <- match(col_names, GB_COLNAMES$old_colnames)
  idx_to_replace <- (!is.na(old_cols))
  col_names[idx_to_replace] <-
    GB_COLNAMES$new_colnames[old_cols[idx_to_replace]]
  return(col_names)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param species_name PARAM_DESCRIPTION
#' @param db_dtype PARAM_DESCRIPTION, Default: 'banding'
#' @param use_spec_code PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_species_data
#' @export
get_species_data <-
  function(species_name,
           db_dtype = "banding",
           use_spec_code = TRUE) {
    db <- get_db(db_type)
    if (use_spec_code) {
      col <- "SPEC"
    } else {
      col <- "species"
    }
    return(db[db[col] == species_name,])
  }

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname check_type
#' @export
check_type <- function(type) {
  type <- tolower(type)
  if (type %in% c("banding", "b")) {
    return("b")
  } else if (type %in% c("recoveries", "r")) {
    return("r")
  } else {
    #TODO : change manual reference for error message
    stop(
      "Unexpected value for 'type' argument. Please refer to ?check_type for accepted values."
    )
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param db_type PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_db
#' @export
get_db <- function(db_type) {
  type <- check_type(db_type)
  if (type == "r") {
    return(gb_recoveries)
  } else if (type == "b") {
    return(gb_banding)
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param type PARAM_DESCRIPTION
#' @param filters PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_filters
#' @export
get_filters <- function(type, filters) {
  type <- check_type(type)
  if (type == "r") {
    base <- RECOVERIES_FILTERS
  } else if (type == "b") {
    base <- BANDING_FILTERS
  } else {
    base <- DEFAULT_LINCOLN_FILTERS
  }

  return(list_update(base, filters))
}

#' @title List the locations available for a species
#' @description This is a convenience function to list all available locations
#' for a given species. By default, a data frame is returned with available
#' countries, states and flyways
#' @param spec The 4 letters short code for the desired species
#' (as found in the SPEC column of the gb_banding dataset)
#' @param columns The desired columns. By default returns all the location
#' related columns: "country_name", "state_name" and "flyway"
#' @param db_type Which database to use, banding or encounters. Default: 'banding'
#' @param sort_by Column by which the results are sorted. Default: NULL, the
#' results are unsorted
#' @return If columns contains more than one element, a data frame with all
#' available combinations of the selected columns.
#' If columns contains only one element, a sorted vector of the unique values
#' of the selected column
#' @details DETAILS
#' @examples
#' get_species_location("ATBR")
#'
#' get_species_location("ATBR", "country_name")
#' # is equivalent to:
#' get_species_countries("ATBR")
#'
#' @rdname get_species_locations
#' @import tidyverse
#' @export
get_species_locations <-
  function(species_code,
           columns = c(
             "b.country_name",
             "b.state_name",
             "b.flyway_name",
             "r.country_name",
             "r.state_name",
             "r.flyway_name"
           ),
           df = NULL,
           db_type = "banding",
           sort_by = NULL) {
    if (is.null(df)) {
      df = get_db(db_type)
    }
    columns = columns[columns %in% colnames(df)]
    if (length(columns) == 0){
      print("No valid columns specified")
      return(NULL)
    }
    if (length(columns) == 1) {
      res = sort(unique(df[df$SPEC == species_code, columns]))
    } else {
      res = df[df$SPEC == species_code, columns] %>% distinct()
      if (!is.null(sort_by)) {
        res = res %>% arrange(.data[[sort_by]])
      }
    }
    return(res)
  }


#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_countries <- function(species_code, db_type="banding") {
  return(get_species_locations(species_code, c("b.country_name", "r.country_name"), db_type=db_type))
}


#' @describeIn get_species_locations Convenience functions to list
#'             all available states for the given species.
#' @export
get_species_states <- function(species_code, db_type="banding") {
  return(get_species_locations(species_code, c("b.state_name", "r.state_name"), db_type=db_type))
}

#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_flyways <- function(species_code, db_type="banding") {
  return(get_species_locations(species_code, c("b.flyway_name", "r.flyway_name"), db_type=db_type))
}


filter_time_period <- function(df, filter, col_name) {
  if (!"start" %in% names(filter)) {
    stop(
      sprintf(
        "Names are provided for '%s' date filter but 'start' is not present",
        col_name
      )
    )
  }
  if (!"end" %in% names(filter)) {
    stop(sprintf(
      "Names are provided for '%s' date filter but 'end' is not present",
      col_name
    ))
  }
  df <-
    df[df[[col_name]] >= filter["start"] &
         df[[col_name]] <= filter["end"],]
  return(df)
}

#' @title Performs filtering on the gb_banding dataset for Lincoln estimates
#' @description For time columns, i.e. 'year', 'month_code' and 'day_code',
#' it is possible to select a period by
#' providing a vector with two named values 'start' and 'end'.
#' If a filter is NULL, it will be skipped
#' @param columns \link[gblincoln]{LINCOLN_BANDING_COLUMNS}, Default: LINCOLN_BANDING_COLUMNS
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lincoln_filter_db
#' @export
lincoln_filter_db <-
  function(filters = NULL,
           df = NULL,
           type = "banding",
           band_type = "all") {
    if (is.null(df)) {
      df = get_db(type)
    }
    # Get filter list and updates it if needed
    filters <- get_filters(type, filters)

    # If no geolocators should be included
    if (band_type == "no_geo") {
      filters$add_info <- ADD_INFO_NO_GEO
    }
    # Select relevant columns
    if ("columns" %in% names(filters)) {
      df <- df[, which(colnames(df) %in% check_columns(filters$columns))]
    }

    # Iterate on all other filters
    for (i in seq_along(filters)) {
      if (is.null(filters[[i]])) {
        next
      }
      col_name <- check_columns(names(filters)[i])
      # Check if the name of the filter is a column of the database
      if (col_name %in% colnames(df)) {
        filter = filters[[i]]
        # Check if the filter is a time filter
        if (col_name %in% TIME_COLUMNS) {
          if (!is.null(names(filter))) {
            # If so, check if it can be a range
            df <- filter_time_period(df, filter, col_name)
            next
          }
        }
        # Else, subset the database based on the filter
        df <- df[df[[col_name]] %in% filter,]
      }
    }
    return(df)
  }
