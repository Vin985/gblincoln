#' @title Rename columns of a dataframe
#' @description This function renames the columns of a dataframe using another
#' dataframe with two columns `old_colnames` and `new_colnames`. Only the
#' columns found in `old_colnames` will be renamed
#' @param df The dataframe whose columns need to be renamed
#' @param columns A dataframe tha contains at least two columns:
#' `old_colnames` which contains the current column names found in df
#' `new_colnames` which contains the new column names
#' @return A dataframe with renamed columns
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rename_columns
#' @export
rename_columns <- function(df, columns) {
  if (is.null(columns)) {
    # If no column names are provided, use the ones in the dataset
    columns <- gb_colnames
  }
  found_columns <- match(colnames(df), columns$old_colnames)
  new_names <- columns[found_columns, "new_colnames"]
  new_columns <- colnames(df)
  new_columns[!is.na(new_names)] <- new_names[!is.na(new_names)]
  colnames(df) <- new_columns
  return(df)
}

#' @title Set age classes
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param age_classes PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname set_age_classes
#' @export
set_age_classes <- function(df, age_classes = NULL) {
  if (is.null(age_classes)) {
    age_classes <- GB_AGE_CLASSES
  }
  i = 1
  for (age_class in age_classes) {
    df$age_short[df$age_code %in% age_class] <- names(age_classes)[i]
    i <- i + 1
  }
  df$age_short[df$age_code == 0] <- NA

  return(df)
}


set_sex_classes <- function(df, sex_classes = NULL) {
  if (is.null(sex_classes)) {
    sex_classes <- GB_SEX_CLASSES
  }
  i = 1
  for (sex_class in sex_classes) {
    df$sex[df$sex_code %in% sex_class] <- names(sex_classes)[i]
    i <- i + 1
  }
  return(df)
}

#' @title Clean a data file directly exported from Gamebirds
#' @description FUNCTION_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param colnames A data frame with old column names (old_colnames) and
#' new column names (new_colnames), Default: NULL
#' @param recoveries PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname clean_dataset
#' @export

clean_dataset <- function(df,
                          rename_columns = TRUE,
                          colnames = NULL,
                          add_age_classes = TRUE,
                          age_classes = NULL,
                          add_sex_classes = TRUE,
                          sex_classes = NULL,
                          correct_recovery_years = TRUE) {
  cleaned <- df
  # rename columns of the dataset
  if (rename_columns) {
    cleaned <- rename_columns(cleaned, colnames)
  }
  # Add age classes and sex classes
  if (is.null(add_age_classes) && "age_code" %in% colnames(cleaned)) {
    cleaned <- set_age_classes(cleaned, age_classes)
  }

  if (add_sex_classes && "sex_code" %in% colnames(cleaned)) {
    cleaned <- set_sex_classes(cleaned, sex_classes)
  }

  # If recoveries dataset, correct the recoveries year
  if (correct_recovery_years && "r.year" %in% colnames(cleaned)) {
    cleaned <- correct_recoveries_years(cleaned)
  }

  return(cleaned)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param path PARAM_DESCRIPTION
#' @param rename_columns PARAM_DESCRIPTION, Default: TRUE
#' @param colnames PARAM_DESCRIPTION, Default: NULL
#' @param add_age_classes PARAM_DESCRIPTION, Default: TRUE
#' @param age_classes PARAM_DESCRIPTION, Default: NULL
#' @param add_sex_classes PARAM_DESCRIPTION, Default: TRUE
#' @param sex_classes PARAM_DESCRIPTION, Default: NULL
#' @param correct_recovery_years PARAM_DESCRIPTION, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname load_dataset
#' @export
load_dataset <- function(path,
                         rename_columns = TRUE,
                         colnames = NULL,
                         add_age_classes = TRUE,
                         age_classes = NULL,
                         add_sex_classes = TRUE,
                         sex_classes = NULL,
                         correct_recovery_years = TRUE,
                         ...) {
  df <- read.csv(path, ...)
  df <-
    clean_dataset(
      df,
      rename_columns,
      colnames,
      add_age_classes,
      age_classes,
      add_sex_classes,
      sex_classes,
      correct_recovery_years
    )
  return(df)
}

#' @title FUNCTION_TITLE
#' @description Correct the recovery year by considering all hunting done before
#' April to belong to the previous year
#' @param df PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname correct_recoveries_years
#' @export
correct_recoveries_years <- function(df) {
  df$r.corrected_year <- df$r.year
  idx <- which(df$r.month < 4)
  df$r.corrected_year[idx] <-  df$r.corrected_year[idx] - 1
  return(df)
}



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
check_columns <- function(col_names, columns = NULL) {
  if (is.null(columns)) {
    # If no column names are provided, use the ones in the dataset
    columns <- gb_colnames
  }
  old_cols <- match(col_names, gb_colnames$old_colnames)
  idx_to_replace <- (!is.na(old_cols))
  col_names[idx_to_replace] <-
    gb_colnames$new_colnames[old_cols[idx_to_replace]]
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
           df = NULL,
           db_dtype = "banding",
           use_spec_code = TRUE) {
    if (is.null(df)) {
      df <- get_db(db_type)
    }
    if (use_spec_code) {
      col <- "SPEC"
    } else {
      col <- "species"
    }
    return(df[df[col] == species_name, ])
  }

#' @title Check database type
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
#' @rdname check_db_type
#' @export
check_db_type <- function(db_type) {
  type <- tolower(db_type)
  if (type %in% c("banding", "b")) {
    return("b")
  } else if (type %in% c("recoveries", "r")) {
    return("r")
  } else {
    #TODO : change manual reference for error message
    stop(
      "Unexpected value for 'db_type' argument. Please refer to ?check_db_type for accepted values."
    )
  }
}


is_recovery <- function(db_type) {
  return (tolower(db_type) %in% c("recoveries", "r"))
}

is_banding <- function(db_type) {
  return (tolower(db_type) %in% c("banding", "b"))
}

is_rho <- function(db_type) {
  return (tolower(db_type) %in% c("rho"))
}

is_harvest <- function(db_type) {
  return (tolower(db_type) %in% c("harvest", "h"))
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
get_filters <- function(db_type, filters) {
  default <- if (is_recovery(db_type)) {
    RECOVERIES_FILTERS
  } else if (is_banding(db_type)) {
    BANDING_FILTERS
  } else {
    DEFAULT_LINCOLN_FILTERS
  }

  return(list_update(default, filters))
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
    if (length(columns) == 0) {
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
get_species_countries <-
  function(species_code, db_type = "banding") {
    return(get_species_locations(
      species_code,
      c("b.country_name", "r.country_name"),
      db_type = db_type
    ))
  }


#' @describeIn get_species_locations Convenience functions to list
#'             all available states for the given species.
#' @export
get_species_states <- function(species_code, db_type = "banding") {
  return(get_species_locations(species_code, c("b.state_name", "r.state_name"), db_type =
                                 db_type))
}

#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_flyways <- function(species_code, db_type = "banding") {
  return(get_species_locations(species_code, c("b.flyway_name", "r.flyway_name"), db_type =
                                 db_type))
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
         df[[col_name]] <= filter["end"], ]
  return(df)
}

#' @title Performs filtering on a dataset
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
#' @rdname filter_database
#' @export
filter_database <-
  function(db,
           filters = NULL,
           db_type = "banding",
           columns = NULL,
           use_default_filters = TRUE) {
    # Get filter list and updates it if needed
    if (use_default_filters) {
      filters <- get_filters(db_type, filters)
    }

    # Select relevant columns
    if ("columns" %in% names(filters)) {
      db <-
        db[, which(colnames(db) %in% check_columns(filters$columns, columns))]
    }

    # Iterate on all other filters
    for (i in seq_along(filters)) {
      if (is.null(filters[[i]])) {
        next
      }
      col_name <- check_columns(names(filters)[i], columns)
      # Check if the name of the filter is a column of the database
      if (col_name %in% colnames(db)) {
        filter = filters[[i]]
        # Check if the filter is a time filter
        if (col_name %in% TIME_COLUMNS) {
          if (!is.null(names(filter))) {
            # If so, check if it can be a range
            db <- filter_time_period(db, filter, col_name)
            next
          }
        }
        # Else, subset the database based on the filter
        db <- db[db[[col_name]] %in% filter, ]
      }
    }
    return(db)
  }
