## code to prepare `gamebirds` dataset goes here
library(openxlsx)
library(tidyverse)

# https://www.pwrc.usgs.gov/BBL/manual/summary.cfm


# Generate replacement columns
# a <- colnames(gb_banding)
# names(a) <- colnames(gb_banding)
# dput(a)

GB_COLNAMES <-
  c(
    add_info = "Add.Info",
    age_code = "Age",
    coordinate_precision = "B.Coordinate.Precision",
    day_code = "B.Day.Code",
    dir = "B.Dir",
    flyway_code = "B.Flyway",
    lat = "B.Lat",
    long = "B.Long",
    month_code = "B.Month",
    region_code = "B.Region",
    year = "B.Year",
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
    age = "Age::VAGE",
    VAI = "AI::VAI",
    btype_vbtype = "BType::VBtype",
    btype_text = "BType::VText",
    location_accuracy_desc = "coord_precision::LOCATION_ACCURACY_DESC",
    day_span = "DayCode::Day.Span",
    how_aged_desc = "How_aged::How.Aged.Description",
    how_sexed_desc = "How_sexed::How.Sexed.Description",
    country_name = "Location_lu::COUNTRY_NAME",
    state_name = "Location_lu::STATE_NAME",
    month = "Month::VMonth",
    permittee = "Permits::Permittee",
    flyway = "Region::Flyway",
    state = "Region::State",
    sex = "Sex::VSEX",
    SPEC = "Species.Game.Birds::SPEC",
    species = "Species.Game.Birds::Species",
    status = "Status::VStatus"
  )

AGE_CLASSES <- list("HY" = c(2, 3, 4), "AHY" = c(1, 5, 6, 7, 8))
SEX_CLASSES <- list("MALE" = 4, "FEMALE" = 5)


set_age_classes <- function(df, age_classes = AGE_CLASSES) {
  i = 1
  for (age_class in age_classes) {
    df$age_short[df$age_code %in% age_class] <- names(age_classes)[i]
    i <- i + 1
  }
  df$age_short[df$age_code == 0] <- NA

  return(df)
}


set_sex_classes <- function(df, sex_classes = SEX_CLASSES) {
  i = 1
  for (sex_class in sex_classes) {
    df$sex[df$sex_code %in% sex_class] <- names(sex_classes)[i]
    i <- i + 1
  }
  return(df)
}

clean_dataset <- function(df, colnames = GB_COLNAMES) {
  cleaned <-
    df %>% rename(colnames) %>% set_age_classes() %>% set_sex_classes()

  return(cleaned)
}


gb_banding <- read.xlsx("data_raw/gamebirds_banding.xlsx")

gb_banding <- clean_dataset(gb_banding)


usethis::use_data(gb_banding, overwrite = TRUE)
