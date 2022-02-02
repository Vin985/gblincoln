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

get_species_data <- function(species_name, db_dtype = "banding") {
  if (db_type == "banding") {
    db = gb_banding
  }
  return(db[db$SPEC == species_name,])
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
           columns = c("country_name", "state_name", "flyway"),
           db_type = "banding",
           sort_by = NULL) {
    if (db_type == "banding") {
      df = gb_banding
    }
    if (length(columns) == 1) {
      res = sort(unique(df[df$SPEC == species_code, columns]))
    } else {
      res = df[df$SPEC == species_code, columns] %>% distinct()
      if (!is.null(sort_by)) {
        print(sort_by)
        res = res %>% arrange(.data[[sort_by]])
      }
    }
    return(res)
  }

#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_countries <- function(species_code) {
  return(get_species_locations(species_code, "country_name"))
}

#' @describeIn get_species_locations Convenience functions to list
#'             all available states for the given species.
#' @export
get_species_states <- function(species_code) {
  return(get_species_locations(species_code, "state_name"))
}

#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_flyways <- function(species_code) {
  return(get_species_locations(species_code, "flyway"))
}


#' @title Default filters to prepare the gb_banding dataset for Lincoln estimates
#' @description This lists contains all the default filters used to preprare
#' gb_banding dataset to calculate Lincoln estimates.
#' @format A list with column names as names and values for the desired filters
#' @export
LINCOLN_FILTERS = list(
  status_code = "3",
  add_info = c("00", "01", "07", "08", "25", "18"),
  sex_code = c(4, 5),
  age_short = c("AHY"),
  columns = c(
    "add_info",
    "age_code",
    "day_code",
    "dir",
    "flyway_code",
    "lat",
    "long",
    "month_code",
    "region_code",
    "year",
    "band_prefix_plus",
    "band_type",
    "count_of_birds",
    "country_code",
    "GISBLat",
    "GISBLong",
    "permit",
    "sex_code",
    "sp_num",
    "state_code",
    "status_code" ,
    "btype_vbtype",
    "btype_text",
    "day_span",
    "state_name",
    "sex" ,
    "SPEC",
    "age_short"
  ),
  month = c(start = 07, end = 08)
)


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

TIME_COLUMNS = c("year", "month_code", "day_code")

#' @title Performs filtering on the gb_banding dataset for Lincoln estimates
#' @description For time columns, i.e. 'year', 'month_code' and 'day_code',
#' it is possible to select a period by
#' providing a vector with two named values 'start' and 'end'
#' @param columns \link[gblincoln]{LINCOLN_BANDING_COLUMNS}, Default: LINCOLN_BANDING_COLUMNS
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lincoln_banding_filter
#' @export
lincoln_banding_filter <- function(filters = NULL, df = NULL) {
  if (is.null(df)) {
    df = gb_banding
  }
  filters <- update_list(LINCOLN_FILTERS, filters)

  if ("columns" %in% names(filters)) {
    df <- df[, filters$columns]
  }

  for (i in seq_along(filters)) {
    col_name <- names(filters)[i]
    if (col_name %in% colnames(df)) {
      filter = filters[[i]]
      if (col_name %in% TIME_COLUMNS) {
        if (!is.null(names(filter))) {
          df <- filter_time_period(df, filter, col_name)
          next
        }
      }
      df <- df[df[[col_name]] %in% filter,]
    }
  }
  return(df)
}
