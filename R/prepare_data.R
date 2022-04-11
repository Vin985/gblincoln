#' @title Rename columns of a dataframe
#' @description This function renames the columns of a dataframe using another
#' reference dataframe with two columns **old_colnames** and **new_colnames**. Only the
#' columns found in **old_colnames** will be renamed. By default, this package
#' ships with default values for renaming that #' can be found in
#' the \code{\link{gb_colnames}} object.
#' @param df The dataframe whose columns need to be renamed
#' @param columns A dataframe tha contains at least two columns:
#' `old_colnames` which contains the current column names found in df
#' `new_colnames` which contains the new column names.
#' @return A dataframe with renamed columns
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
#' @description Classify age codes found in Gamebirds in subclasses. Subclasses
#' are defined in the **age_classes** argument where each class is associated to
#' one or several age codes.
#' @param df The dataframe in which we want to add age classes
#' @param age_classes A list that associates a new class with the age codes
#' found in the "Age" column of the Gamebirds dataset, Default: NULL
#' @return The modified dataframe with a new column named **age_short**
#' @details The age codes are found in the Gamebirds "Age" column
#' (renamed "age_code" by default in the dataframe loaded by the package).
#'
#' If no value is given for the `age_classes` argument, the following values are
#' used: age_classes = list("HY" = c(2, 3, 4), "AHY" = c(1, 5, 6, 7, 8)).
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


#' @title Set sex classes
#' @description Classify sex codes found in Gamebirds in subclasses. Subclasses
#' are defined in the **sex_classes** argument where each class is associated to
#' one or several sex codes.
#' @param df The dataframe in which we want to add sex classes
#' @param sex_classes A list that associates a new class with the sex codes
#' found in the "Sex" column of the Gamebirds dataset, Default: NULL
#' @return The modified dataframe with the sex classes placed in the column
#' **sex**
#' @details The sex codes are found in the Gamebirds "Sex" column
#' (renamed "sex_code" by default in the dataframe loaded by the package).
#'
#' If no value is given for the `sex_classes` argument, the following values are
#' used: sex_classes = list("MALE" = 4, "FEMALE" = 5).
#' @rdname set_sex_classes
#' @export
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

#' @title Clean datasets from Gamebirds for Lincoln estimations
#' @description This function takes a dataframe directly exported from Gamebirds
#' and prepares it to be used by the *gblincoln* package. It performs the
#' following actions in the following order:
#'  * Rename columns (see \code{\link{rename_columns}})
#'  * Add age classes (see \code{\link{set_age_classes}})
#'  * Add sex classes (see \code{\link{set_sex_classes}})
#'  * Correct recovery years (see \code{\link{correct_recovery_years}})
#' @param df The dataframe to clean. Must contain untouched data coming from
#' Gamebirds.
#' @param rename_columns Should the columns be renamed? Default: TRUE
#' @param colnames Data frame. The columns to rename. Will be passed as the `columns`
#' argument to the \code{\link{rename_columns}} function, Default: NULL
#' @param add_age_classes Should the function add age classes? Default: TRUE
#' @param age_classes The age classes that will be passed as the `age_classes`
#' argument of the (see \code{\link{set_age_classes}}) function. Default: NULL
#' @param add_sex_classes Should the function add sex classes? Default: TRUE
#' @param sex_classes The sex classes that will be passed as the `sex_classes`
#' argument of the (see \code{\link{set_sex_classes}}) function. Default: NULL
#' @param correct_recovery_years Should the recovery years be corrected? Default: TRUE
#' @return The modified dataframe
#' @details
#'  * Adding age classes will only work if the **age_code** column is present in
#'  the dataframe.
#'  * Adding sex classes will only work if the **sex_code** column is present in
#'  the dataframe.
#'  * Correcting recovery years will only work if the **r.year** column is present
#'  in the dataframe
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
  if (add_age_classes &&
      "age_code" %in% colnames(cleaned)) {
    cleaned <- set_age_classes(cleaned, age_classes)
  }

  if (add_sex_classes && "sex_code" %in% colnames(cleaned)) {
    cleaned <- set_sex_classes(cleaned, sex_classes)
  }

  # If recoveries dataset, correct the recoveries year
  if (correct_recovery_years && "r.year" %in% colnames(cleaned)) {
    cleaned <- correct_recovery_years(cleaned)
  }

  return(cleaned)
}



#' @title Loads a csv dataset exported from Gamebirds
#' @description This functions loads a csv dataset exported from Gamebirds and
#' cleans it to be used by the **gblincoiln** package. This is the recommended
#' way to import data for use by the package.
#' @param path Path to the data file. Should point to a csv file.
#' @param rename_columns Should the columns be renamed? Default: TRUE
#' @param colnames Data frame. The columns to rename. Will be passed as the `columns`
#' argument to the \code{\link{rename_columns}} function, Default: NULL
#' @param add_age_classes Should the function add age classes? Default: TRUE
#' @param age_classes The age classes that will be passed as the `age_classes`
#' argument of the (see \code{\link{set_age_classes}}) function. Default: NULL
#' @param add_sex_classes Should the function add sex classes? Default: TRUE
#' @param sex_classes The sex classes that will be passed as the `sex_classes`
#' argument of the (see \code{\link{set_sex_classes}}) function. Default: NULL
#' @param correct_recovery_years Should the recovery years be corrected? Default: TRUE
#' @param ... Additional arguments passed to the \code{\link{read.csv}} function
#' @return A dataframe formatted to use in the **gblincoln** package
#' @seealso \code{\link{clean_dataset}}
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

#' @title Correct recovery years
#' @description Correct the recovery year by considering all hunting done before
#' April to belong to the previous year.
#' @param df The data frame to modify
#' @return The modified dataframe
#' @seealso \code{\link{clean_dataset}}
#' @rdname correct_recovery_years
#' @export
correct_recovery_years <- function(df) {
  df$r.corrected_year <- df$r.year
  idx <- which(df$r.month < 4)
  df$r.corrected_year[idx] <-  df$r.corrected_year[idx] - 1
  return(df)
}



#' @title Check column names for filters
#' @description When creating filters, either new columns names defined by the
#' **gblincoln** package or column names provided by Gamebirds are accepted.
#' This function takes as input a vector of column names and replaces all
#' Gamebirds columns found by the package ones.
#' @param col_names A vector of column names
#' @param columns A dataframe that contains equivalence between Gamebirds
#' columns and the package ones. Default: NULL
#' @return A vector of same length as *col_names* but containing only renamed
#' columns
#' @seealso \code{\link{rename_columns}}
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


#' @title Get data for a given species
#' @description Convenience function to extract data for a given species. The
#' species can be selected either by its 4 letters alpha code (use_spec_code=TRUE)
#' or its full name (use_spec_code=FALSE
#' @param species_name The species name. Should be either a 4 letters alpha
#' code (as found in the SPEC column of the Gamebirds dataset) if *use_spec_code*
#' is set to TRUE, or the full name as found in the **Species** column.
#' @param df The dataframe to subset
#' @param use_spec_code Use the species code?, Default: TRUE
#' @return A subset of *df* containing only the target species
#' @rdname get_species_data
#' @export
get_species_data <-
  function(species_name,
           df,
           use_spec_code = TRUE) {
    if (use_spec_code) {
      col <- "SPEC"
    } else {
      col <- "species"
    }
    return(df[df[col] == species_name, ])
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

#' @title Get database type
#' @description Get a unique consistent database identifier type depending on the
#' code provided as input
#' @param db_type A character string describing a database type
#' @param short Return a short identifier or not, Default: FALSE
#' @return A unique identifier describing the database type
#' @details There are four accepted database types: *banding*, *recoveries*,
#' *harvest* and *rho* (reporting probabilities).
#' Here are the possible values db_type can take and the returning values. Note
#' that the long values of db_type are case insensitive.
#'
#' | **Database** | **db_type** | **Short id** | **Long id** |
#' | :--- | :---: | :---: | :---: |
#' | Banding | "banding" / "b" | "b" | "banding" |
#' | Recoveries | "recoveries" / "r" | "r" | "recoveries" |
#' | Reporting probabilities | "rho" | "rho" | "rho" |
#' | Harvest |  "harvest" / "h" | "h" | "harvest" |
#'
#'
#' @examples
#' get_db_type("banding")
#' get_db_type("HARVEST", short = TRUE)
#'
#' @rdname get_db_type
#' @export
get_db_type <- function(db_type, short = FALSE) {
  res <- if (is_banding(db_type)) {
    ifelse(short, "b", "banding")
  } else if (is_recovery(db_type)) {
    ifelse(short, "r", "recoveries")
  } else if (is_rho(db_type)) {
    "rho"
  } else if (is_harvest(db_type)) {
    ifelse(short, "h", "harvest")
  } else {
    stop("Unrecognized database type")
  }
  return (res)
}

#' @title List the locations available for a species
#' @description This is a convenience function to list all available locations
#' for a given species. By default, a data frame is returned with available
#' countries, states and flyways
#' @param df The dataframe in which to search
#' @param species_code The 4 letters short code for the desired species
#' (as found in the SPEC column of the gb_banding dataset)
#' @param type The type of data desired. Can be one of "country", "state" or
#' "flyway", Default: c("country", "state", "flyway")
#' @param db_type The type of data desired, banding or recovery. Can take any
#' value accepted by \code{\link{get_db_type}} but will only work for banding
#' or recovery data, Default: c("b", "r")
#' @param columns The desired columns. By default returns all the location
#' related columns: "country_name", "state_name" and "flyway"
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
  function(df,
           species_code,
           type = c("country", "state", "flyway"),
           db_types = c("b", "r"),
           columns = NULL,
           return_names = TRUE,
           return_codes = TRUE,
           sort_by = NULL) {
    if (is.null(columns)) {
      columns = NULL
      for (db_type in db_types) {
        str_type = get_db_type(db_type, short = TRUE)
        for (loc in type) {
          if (!loc %in% c("country", "state", "flyway")) {
            warning(sprintf("Unrecognized type %s. Skipping."))
          }
          if (return_names) {
            columns = c(columns, paste0(str_type, ".", loc, "_name"))
          }
          if (return_codes) {
            columns = c(columns, paste0(str_type, ".", loc, "_code"))
          }
        }
      }
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
  function(df, species_code, ...) {
    return(get_species_locations(df,
                                 species_code,
                                 type = "country", ...))
  }


#' @describeIn get_species_locations Convenience functions to list
#'             all available states for the given species.
#' @export
get_species_states <- function(df, species_code, ...) {
  return(get_species_locations(df, species_code, type = "state", ...))
}

#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_flyways <- function(df, species_code, ...) {
  return(get_species_locations(df, species_code, type = "flyway", ...))
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param filters PARAM_DESCRIPTION
#' @param db_type PARAM_DESCRIPTION
#' @param filters_first PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_db_filters
#' @export
add_db_filters <-
  function(filters, db_type, filters_first = FALSE) {
    opt_name <- paste0(get_db_type(db_type), "_filters")
    if (opt_name %in% names(filters)) {
      filters <- list_update(filters, filters[[opt_name]], filters_first)
    }
    filters[endsWith(names(filters), "_filters")] <- NULL
    return(filters)
  }


#' @title Performs filtering on a dataset
#' @description For time columns, i.e. 'year', 'month_code' and 'day_code',
#' it is possible to select a period by
#' providing a vector with two named values 'start' and 'end'.
#' If a filter is NULL, it will be skipped
#' @param db PARAM_DESCRIPTION
#' @param filters PARAM_DESCRIPTION, Default: NULL
#' @param columns PARAM_DESCRIPTION, Default: NULL
#' @param use_default_filters PARAM_DESCRIPTION, Default: TRUE
#' @param db_type PARAM_DESCRIPTION, Default: NULL
#' @param filters_first PARAM_DESCRIPTION, Default: FALSE
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
           columns = NULL,
           use_default_filters = TRUE,
           db_type = NULL,
           filters_first = FALSE) {
    # Get filter list and updates it if needed
    if (use_default_filters) {
      filters <-
        list_update(DEFAULT_LINCOLN_FILTERS, filters, filters_first)
    }

    if (!is.null(db_type)) {
      filters <- add_db_filters(filters, db_type, filters_first)
    }

    # Select relevant columns
    if ("columns" %in% names(filters)) {
      db <-
        db[, which(colnames(db) %in% check_columns(filters$columns, columns))]
      filters["columns"] <- NULL
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
