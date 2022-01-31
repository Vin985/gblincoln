#' @title Gamebirds Dataset
#' @description DATASET_DESCRIPTION
#' @format A data frame with 16068 rows and 43 variables:
#' \describe{
#'   \item{\code{Add.Info}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Age}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B.Coordinate.Precision}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B.Day.Code}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B.Dir}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B.Flyway}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B.Lat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B.Long}}{double COLUMN_DESCRIPTION}
#'   \item{\code{B.Month}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B.Region}}{character COLUMN_DESCRIPTION}
#'   \item{\code{B.Year}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Band.Prefix.Plus}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Band.Size}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Band.Type}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Count.of.Birds}}{double COLUMN_DESCRIPTION}
#'   \item{\code{Country_code}}{character COLUMN_DESCRIPTION}
#'   \item{\code{GISBLat}}{double COLUMN_DESCRIPTION}
#'   \item{\code{GISBLong}}{double COLUMN_DESCRIPTION}
#'   \item{\code{How.Aged}}{character COLUMN_DESCRIPTION}
#'   \item{\code{How.Sexed}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Permit}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Sex}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Sp..Num.}}{character COLUMN_DESCRIPTION}
#'   \item{\code{State_code}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Status}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Age::VAGE}}{character COLUMN_DESCRIPTION}
#'   \item{\code{AI::VAI}}{character COLUMN_DESCRIPTION}
#'   \item{\code{BType::VBtype}}{character COLUMN_DESCRIPTION}
#'   \item{\code{BType::VText}}{character COLUMN_DESCRIPTION}
#'   \item{\code{coord_precision::LOCATION_ACCURACY_DESC}}{character COLUMN_DESCRIPTION}
#'   \item{\code{DayCode::Day.Span}}{character COLUMN_DESCRIPTION}
#'   \item{\code{How_aged::How.Aged.Description}}{character COLUMN_DESCRIPTION}
#'   \item{\code{How_sexed::How.Sexed.Description}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Location_lu::COUNTRY_NAME}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Location_lu::STATE_NAME}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Month::VMonth}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Permits::Permittee}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region::Flyway}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Region::State}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Sex::VSEX}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Species.Game.Birds::SPEC}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Species.Game.Birds::Species}}{character COLUMN_DESCRIPTION}
#'   \item{\code{Status::VStatus}}{character COLUMN_DESCRIPTION}
#'}
"gb_banding"

get_species_data <- function(species_name, db_dtype = "banding") {
  if (db_type == "banding") {
    db = gb_banding
  }
  return(db[db$SPEC == species_name, ])
}

#' @title List the locations available for a species
#' @description This is a convenience function to list all available locations
#' for a given species. By default, a data frame is returned with available
#' countries, states and flyways
#' @param spec The 4 letters short code for the desired species
#' (as found in the SPEC column of the gb_banding dataset)
#' @param columns PARAM_DESCRIPTION, Default: c("country_name", "state_name", "flyway")
#' @param db_type PARAM_DESCRIPTION, Default: 'banding'
#' @param sort_by PARAM_DESCRIPTION, Default: NULL
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
#' @export
get_species_locations <-
  function(spec,
           columns = c("country_name", "state_name", "flyway"),
           db_type = "banding",
           sort_by = NULL) {
    if (db_type == "banding") {
      db = gb_banding
    }
    if (length(columns) == 1) {
      res = sort(unique(db[db$SPEC == spec, columns]))
    } else {
      res = db[db$SPEC == species_name, columns] %>% distinct()
      if (!is.null(sort_by)) {
        print(sort_by)
        res = res %>% arrange(.data[[sort_by]])
      }
    }
    return(res)
  }

#' @inheritParams get_species_locations
#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_countries <- function(spec) {
  return(get_species_locations(spec, "country_name"))
}

#' @inheritParams get_species_locations
#' @describeIn get_species_locations Convenience functions to list
#'             all available states for the given species.
#' @export
get_species_states <- function(spec) {
  return(get_species_locations(spec, "state_name"))
}

#' @inheritParams get_species_locations
#' @describeIn get_species_locations Convenience functions to list
#'             all available countries for the given species.
#' @export
get_species_flyways <- function(spec) {
  return(get_species_locations(spec, "flyway"))
}
