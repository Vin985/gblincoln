
GB_COLNAMES <-
  c(
    add_info = "Add.Info",
    age_code = "Age",
    b.coordinate_precision = "B.Coordinate.Precision",
    b.day_code = "B.Day.Code",
    b.dir = "B.Dir",
    b.flyway_code = "B.Flyway",
    b.lat = "B.Lat",
    b.long = "B.Long",
    b.month_code = "B.Month",
    b.region_code = "B.Region",
    b.year = "B.Year",
    band_prefix_plus = "Band.Prefix.Plus",
    band_size = "Band.Size",
    band_type = "Band.Type",
    count_of_birds = "Count.of.Birds",
    country_code = "Country_code",
    GISBLat = "GISBLat",
    GISBLong = "GISBLong",
    how_aged = "How.Aged",
    how_sexed = "How.Sexed",
    permit = "Permit",
    sex_code = "Sex",
    sp_num = "Sp..Num.",
    state_code = "State_code",
    status_code = "Status",
    age = "Age..VAGE",
    VAI = "AI..VAI",
    btype_vbtype = "BType..VBtype",
    btype_text = "BType..VText",
    location_accuracy_desc = "coord_precision..LOCATION_ACCURACY_DESC",
    day_span = "DayCode..Day.Span",
    how_aged_desc = "How_aged..How.Aged.Description",
    how_sexed_desc = "How_sexed..How.Sexed.Description",
    country_name = "Location_lu..COUNTRY_NAME",
    state_name = "Location_lu..STATE_NAME",
    month = "Month..VMonth",
    permittee = "Permits..Permittee",
    flyway = "Region..Flyway",
    state = "Region..State",
    sex = "Sex..VSEX",
    SPEC = "Species.Game.Birds..SPEC",
    species = "Species.Game.Birds..Species",
    status = "Status..VStatus"
  )

SEX_CLASSES <- list("MALE" = 4, "FEMALE" = 5)
AGE_CLASSES <- list("HY" = c(2, 3, 4), "AHY" = c(1, 5, 6, 7, 8))

DEFAULT_ADD_INFO_NO_GEO <- c()
DEFAULT_ADD_INFO_ALL <- c()
DEFAULT_BANDING_MONTHS <- c()
DEFAULT_RECOVERIES_MONTHS <- c()

#' @title Default filters to prepare the gb_banding dataset for Lincoln estimates
#' @description This lists contains all the default filters used to preprare
#' gb_banding dataset to calculate Lincoln estimates.
#' @format A list with column names as names and values for the desired filters
#' @export
DEFAULT_LINCOLN_FILTERS = list(
  status_code = "3",
  add_info = c(00, 01, 07, 08, 25, 18),
  sex_code = c(4, 5),
  age_short = c("AHY"),
  columns = c(
    "add_info",
    "age_code",
    "b.day_code",
    "b.dir",
    "b.flyway_code",
    "b.lat",
    "b.long",
    "b.month_code",
    "b.region_code",
    "b.year",
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
  b.month_code = c(start = 07, end = 08)
)

BANDING_FILTERS <- DEFAULT_LINCOLN_FILTERS
RECOVERIES_FILTERS <- list_update(DEFAULT_LINCOLN_FILTERS, list())
TIME_COLUMNS = c("b.year", "b.month_code", "b.day_code")
