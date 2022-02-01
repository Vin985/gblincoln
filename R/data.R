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


#' @title gb_banding dataset columns for Lincoln estimates
#' @description Columns selected for filtering the gb_banding dataset to calculate
#' Lincoln estimates
#' @format A character vector with 1 value
LINCOLN_FILTERS = list(
  status_code = "3",
  add_info = c("00", "01", "07", "08", "25", "18"),
  sex_code = c(4,5),
  age_short = c("AHY"),
  columns = c("SPEC", "country_code")
)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
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
lincoln_banding_filter <- function(df=NULL, filters=NULL){
  if (is.null(df)){
    df = gb_banding
  }
  filters <- update_list(filters, LINCOLN_FILTERS)

  if ("columns" %in% names(filters)){
    df <- df[, filters$columns]
  }

  for(i in seq_along(filters)){
    col_name <- names(filters)[i]
    if (col_name %in% colnames(db)){
      filter = filters[[i]]
      print(col_name)
      print(filter)
      # df <- df[!is.na(df[col_name]),]
      df <- df[df[[col_name]] %in% filter, ]
      print(df)
    }


  }
  print(df)

  # df <- df %>%
  #   filter(Status==3) %>% # normal, wild birds
  #   filter(Add.Info %in% c(00, 01, 07, 08, 25, 18),  #00 (normal), 01 (color band), 07 (double bands), 08 (temp marker-paint or dye), 25 (geolocators), 18 (blood sampled)
  #          Sex%in% c(4,5)
  #   ) %>%
  #   filter(TAGE=='AHY') %>%
  #
  # ATBRBAND2 <- ATBRBAND1 %>%
  #   filter(!is.na(TAGE)) %>%
  #   filter(LOC %in% c("Nunavut"))%>%
  #   filter(B.Month>6)%>%
  #   filter(B.Month<9)%>%
  #   select(-Sex..VSEX, -B.Coordinate.Precision, -Band.Size, -How_aged..How.Aged.Description, -How.Sexed, -How.Aged, -Age..VAGE,
  #          -AI..VAI, -coord_precision..LOCATION_ACCURACY_DESC, -DayCode..Day.Span, -How_sexed..How.Sexed.Description,
  #          -Location_lu..COUNTRY_NAME, -Month..VMonth, -Permits..Permittee, -Region..Flyway, -Status..VStatus, -Region..State, -Object_Name, -MARPLOT.Layer.Name, -MARPLOT.Map.Name, -symbol, -color, -idmarplot, -Species.Game.Birds..Species)
}
