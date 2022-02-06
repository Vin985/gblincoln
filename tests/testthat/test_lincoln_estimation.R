

ATBRBAND <-
  read.csv("/mnt/win/dev/EC/Lincoln/data/ATBR_allbandings.csv",
           stringsAsFactors = FALSE)
names(ATBRBAND) <-
  str_replace_all(names(ATBRBAND), c(" " = "." , "," = ""))
table(ATBRBAND$B.Year, useNA = "always")
ATBRBAND$TAGE[ATBRBAND$Age == 1] <- "AHY"
ATBRBAND$TAGE[ATBRBAND$Age == 5] <- "AHY"
ATBRBAND$TAGE[ATBRBAND$Age == 6] <- "AHY"
ATBRBAND$TAGE[ATBRBAND$Age == 7] <- "AHY"
ATBRBAND$TAGE[ATBRBAND$Age == 8] <- "AHY"
ATBRBAND$TAGE[ATBRBAND$Age == 2] <- "HY"
ATBRBAND$TAGE[ATBRBAND$Age == 3] <- "HY"
ATBRBAND$TAGE[ATBRBAND$Age == 4] <- "HY"
ATBRBAND$TAGE[ATBRBAND$Age == 0] <- NA
ATBRBAND$TSEX[ATBRBAND$Sex == 4] <- "MALE"
ATBRBAND$TSEX[ATBRBAND$Sex == 5] <- "FEMALE"

ATBRBAND1 <- ATBRBAND %>%
  filter(Status == 3) %>% # normal, wild birds
  filter(Add.Info %in% c(00, 01, 07, 08, 25, 18),  #00 (normal), 01 (color band), 07 (double bands), 08 (temp marker-paint or dye), 25 (geolocators), 18 (blood sampled)
         Sex %in% c(4, 5)) %>%
  filter(TAGE == 'AHY') %>%
  rename(LOC = Location_lu..STATE_NAME)

ATBRBAND2 <- ATBRBAND1 %>%
  filter(!is.na(TAGE)) %>%
  filter(LOC %in% c("Nunavut")) %>%
  filter(B.Month > 6) %>%
  filter(B.Month < 9) %>%
  select(
    -Sex..VSEX,-B.Coordinate.Precision,-Band.Size,-How_aged..How.Aged.Description,-How.Sexed,-How.Aged,-Age..VAGE,
    -AI..VAI,-coord_precision..LOCATION_ACCURACY_DESC,-DayCode..Day.Span,-How_sexed..How.Sexed.Description,
    -Location_lu..COUNTRY_NAME,-Month..VMonth,-Permits..Permittee,-Region..Flyway,-Status..VStatus,-Region..State,-Object_Name,-MARPLOT.Layer.Name,-MARPLOT.Map.Name,-symbol,-color,-idmarplot,-Species.Game.Birds..Species
  )

ATBRBANDsum <- ATBRBAND2 %>%
  filter(B.Year %in% 2000:2019) %>%
  group_by(B.Year) %>%
  summarise(total = sum(Count.of.Birds))
ATBRBANDsum

test_that("Summarize banding", {
  filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  banding_db <- lincoln_filter_db(filter)
  pkg_sum <- summarize_banding(banding_db)
  colnames(pkg_sum) <- c("B.Year", "total")
  expect_equal(pkg_sum, ATBRBANDsum)
})
