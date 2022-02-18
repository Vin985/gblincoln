#' @export
GB_COLNAMES <- read.csv('data_raw/colnames.csv')

#' @export
REPORTING_PROBABILITIES <- read.csv('data_raw/reporting_probabilities.csv')

SEX_CLASSES <- list("MALE" = 4, "FEMALE" = 5)
AGE_CLASSES <- list("HY" = c(2, 3, 4), "AHY" = c(1, 5, 6, 7, 8))

ADD_INFO_NO_GEO <- c(00, 01, 07)
ADD_INFO_ALL <- c(00, 01, 07, 08, 25, 18)
GEO_YEARS <- 2018:2019

DEFAULT_BANDING_MONTHS <- c()
DEFAULT_RECOVERIES_MONTHS <- c()

#' @title Default filters to prepare the gb_banding dataset for Lincoln estimates
#' @description This lists contains all the default filters used to preprare
#' gb_banding dataset to calculate Lincoln estimates.
#' add_info: 00 (normal), 01 (color band), 07 (double bands),
#' 08 (temp marker-paint or dye), 25 (geolocators), 18 (blood sampled)
#' @format A list with column names as names and values for the desired filters
#' @export
DEFAULT_LINCOLN_FILTERS = list(
  status_code = "3",
  # normal, wild birds
  add_info = c(00, 01, 07, 08, 25, 18),
  # all band types
  sex_code = c(4, 5),
  # Male and females
  age_short = c("AHY"),
  columns = c(
    "add_info",
    "age_code",
    "b.day_code",
    "b.dir",
    "b.flyway_code",
    "b.lat",
    "b.long",
    "b.month",
    "b.region_code",
    "b.year",
    "band_prefix_plus",
    "band_type",
    "count_of_birds",
    "b.country_code",
    "GISBLat",
    "GISBLong",
    "permit",
    "sex_code",
    "sp_num",
    "b.state_code",
    "status_code" ,
    "btype_vbtype",
    "btype_text",
    "day_span",
    "b.state_name",
    "sex" ,
    "SPEC",
    "age_short",
    "e.object_name",
    "GISRLong",
    "GISRLat",
    "band",
    "e_country_code",
    "r.day_code",
    "r.flyway_code",
    "r.lat",
    "r.long",
    "r.month",
    "r.region",
    "r.year",
    "e.country_name",
    "r.flyway_name",
    "r.state_code",
    "r.state_name",
    "r.corrected_year",
    "how_obt"
  ),
  how_obt = 1,
  # Shot only
  b.month = c(7, 8),
  r.month = c(9, 10, 11, 12, 1, 2, 3)
)

BANDING_FILTERS <- list_update(DEFAULT_LINCOLN_FILTERS, list(b.year= 2000:2019))
RECOVERIES_FILTERS <- list_update(DEFAULT_LINCOLN_FILTERS, list(r.corrected_year=2000:2019))
TIME_COLUMNS = c("b.year", "b.month", "b.day", "r.year", "r.month", "r.day")
