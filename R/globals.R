
#
# GB_COLNAMES <-
#   c(
#     add_info = "Add.Info",
#     age_code = "Age",
#     b.coordinate_precision = "B.Coordinate.Precision",
#     b.day_code = "B.Day.Code",
#     b.dir = "B.Dir",
#     b.flyway_code = "B.Flyway",
#     b.lat = "B.Lat",
#     b.long = "B.Long",
#     b.month_code = "B.Month",
#     b.region_code = "B.Region",
#     b.year = "B.Year",
#     band_prefix_plus = "Band.Prefix.Plus",
#     band_size = "Band.Size",
#     band_type = "Band.Type",
#     count_of_birds = "Count.of.Birds",
#     country_code = "Country_code",
#     GISBLat = "GISBLat",
#     GISBLong = "GISBLong",
#     how_aged = "How.Aged",
#     how_sexed = "How.Sexed",
#     permit = "Permit",
#     sex_code = "Sex",
#     sp_num = "Sp..Num.",
#     state_code = "State_code",
#     status_code = "Status",
#     age = "Age..VAGE",
#     VAI = "AI..VAI",
#     btype_vbtype = "BType..VBtype",
#     btype_text = "BType..VText",
#     location_accuracy_desc = "coord_precision..LOCATION_ACCURACY_DESC",
#     day_span = "DayCode..Day.Span",
#     how_aged_desc = "How_aged..How.Aged.Description",
#     how_sexed_desc = "How_sexed..How.Sexed.Description",
#     country_name = "Location_lu..COUNTRY_NAME",
#     state_name = "Location_lu..STATE_NAME",
#     month = "Month..VMonth",
#     permittee = "Permits..Permittee",
#     flyway = "Region..Flyway",
#     state = "Region..State",
#     sex = "Sex..VSEX",
#     SPEC = "Species.Game.Birds..SPEC",
#     species = "Species.Game.Birds..Species",
#     status = "Status..VStatus",
#     GISRLong="GISRLong",
#     GISRLat="GISRLat",
#     object_name.r="Object_Name.E",
#     MARPLOT_layer_name="MARPLOT.Layer.Name",
#     MARPLOT_map_name="MARPLOT.Map.Name",
#     symbol="symbol",
#     color="color",
#     idmarplot="idmarplot",
#     b.day="B.Day",
#     b.country_code = "b_country_code",
#     b.state_code="b_state_code",
#     band="Band",
#     band_type_current = "Band.Type.Current",
#     band_type_orig = "Band.Type.Orig",
#     cardinal_direction = "Cardinal.Direction",
#     distance = "Distance",
#     e.country_code = "e_country_code",
#     e.state_code = "e_state_code",
#     how_obt = "How.Obt",
#     hunt_season_surv="Hunt..Season.Surv.",
#     b.marker_desc="Marker_Desc_bndg",
#     e.marker_desc="Marker_Desc_enc",
#     e.min_age_at_enc="MIN_AGE_AT_ENC",
#     "Pres..Cond.",
#     r.coordinate_precision="R.Coordinate.Precision",
#     r.create_date_month="R.Create.date.Month",
#     r.create_date.year="R.Create.date.Year",
#     r.day="R.Day",
#     r.dir="R.Dir",
#     r.flyway="R.Flyway",
#     r.lat="R.Lat",
#     r.long="R.Long",
#     r.month="R.Month",
#     r.region="R.Region",
#     r.year="R.Year",
#     replaced_band_code="Replaced.Band.Code",
#     replaced_band_translated="Replaced.Band.Translated",
#     same.block="Same.Block",
#     who_reported="Who.Reported",
#     "Why..pre.1994...Report.Method..after.1994.",
#     b.location_accuracy_desc="B.Coord.Precision..LOCATION_ACCURACY_DESC",
#     b.day_vrday="BDay..VRDay",
#     b.month_name="BMonth..VMonth",
#     b.state_code="BRegion..STA",
#     b.state_name="BRegion..State",
#     btype_current_vbtype="BType.Current..VBtype",
#     btype_current_text="BType.Current..VText",
#     condition_band_status="Condition..VBandStatus",
#     condition_condition="Condition..VCondition",
#     how_obt_text="How.Obt..VHow",
#     location_lu_enc_country_name="Location_lu_enc..COUNTRY_NAME",
#     location_lu_enc_state_name="Location_lu_enc..STATE_NAME",
#     r.location_accuracy_desc="R.Coord.Precision..LOCATION_ACCURACY_DESC",
#     r.dat_vrday="RDay..VRDay",
#     rept_method_text="Rept.Mthd..VRept.Mthd",
#     r.flyway="RFly..Flyway",
#     r.month_name="RMonth..VMonth",
#     r.state_code="RRegion..STA",
#     r.state_name="RRegion..State",
#     who="Who..VWho"
# )

#' @export
GB_COLNAMES <- read.csv('data_raw/colnames.csv')

SEX_CLASSES <- list("MALE" = 4, "FEMALE" = 5)
AGE_CLASSES <- list("HY" = c(2, 3, 4), "AHY" = c(1, 5, 6, 7, 8))

ADD_INFO_NO_GEO <- c(00, 01, 07)
ADD_INFO_ALL <- c(00, 01, 07, 08, 25, 18)
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
  how_obt = 1,
  # Shot only
  b.month_code = c(start = 07, end = 08),
  r.month_code = c(9, 10, 11, 12, 1, 2, 3)
)

BANDING_FILTERS <- list_update(DEFAULT_LINCOLN_FILTERS, list())
RECOVERIES_FILTERS <- list_update(DEFAULT_LINCOLN_FILTERS, list())
TIME_COLUMNS = c("b.year", "b.month_code", "b.day_code")
