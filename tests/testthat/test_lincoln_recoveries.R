


ATBRRECOV <- read.csv("/mnt/win/dev/EC/Lincoln/data/ATBR_allrecovs.csv",stringsAsFactors = FALSE)
names(ATBRRECOV)<-str_replace_all(names(ATBRRECOV), c(" " = "." , "," = "" ))

ATBRRECOV$TAGE[ATBRRECOV$Age == 1] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 5] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 6] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 7] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 8] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 2] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 3] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 4] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 0] <- NA
ATBRRECOV$TSEX[ATBRRECOV$Sex== 4] <-"MALE"
ATBRRECOV$TSEX[ATBRRECOV$Sex== 5] <-"FEMALE"
ATBRRECOV$COUNT<- 1

ATBRRECOV1 <- ATBRRECOV %>%
  filter(Status==3) %>% # normal, wild birds
  filter(Add.Info %in% c(00, 01, 07, 08, 25, 18),  #00 (normal), 01 (color band), 07 (double bands), 08 (temp marker-paint or dye), 25 (geolocators), 18 (blood sampled)
         Sex%in% c(4,5)
  ) %>%
  filter(TAGE=='AHY') %>%
  filter(How.Obt==1) %>%
  filter(e_country_code=='US') %>%
  filter(R.Flyway==1) %>%
  rename(LOC = Location_lu..STATE_NAME
  )

ATBRRECOV2 <- ATBRRECOV1 %>%
  filter(!is.na(TAGE)) %>%
  filter(LOC %in% c("Nunavut"))%>%
  filter(R.Month %in% c(9,10,11,12,1,2,3)) %>%
  filter(B.Month %in% c(7,8))%>%
  select(-B.Coordinate.Precision, -b_country_code, -b_state_code, -Band.Size, -Band.Type.Current, -Band.Type.Orig,
         -Cardinal.Direction, -Distance, -e_state_code, -How.Aged, -How.Obt, -How.Sexed, -Hunt..Season.Surv.,
         -Marker_Desc_bndg, -Marker_Desc_enc, -MIN_AGE_AT_ENC, -other_bands, -Permit, -Pres..Cond., -R.Coordinate.Precision,
         -R.Create.date.Month, -R.Create.date.Year, -R.Dir, -Replaced.Band.Code, -Replaced.Band.Translated, -Same.Block,
         -Who.Reported, -Why..pre.1994...Report.Method..after.1994., -Age..VAGE, -AI..VAI, -B.Coord.Precision..LOCATION_ACCURACY_DESC, -BDay..VRDay,
         -BMonth..VMonth, -BRegion..STA, -BRegion..State, -BType.Current..VBtype, -BType.Current..VText, -BType..VBtype, -BType..VText,
         -Condition..VBandStatus, -Condition..VCondition, -How_aged..How.Aged.Description,
         -How_sexed..How.Sexed.Description, -Location_lu_enc..STATE_NAME, -Location_lu..COUNTRY_NAME,
         -Permits..Permittee, -R.Coord.Precision..LOCATION_ACCURACY_DESC, -RDay..VRDay, -Region..Flyway, -How.Obt..VHow,
         -Rept.Mthd..VRept.Mthd, -RMonth..VMonth, -Sex..VSEX, -Species.Game.Birds..SPEC, -Species.Game.Birds..Species, -Status..VStatus,
         -Who..VWho)

CHECK<-ATBRRECOV2%>%
  group_by(R.Month)%>%
  summarise(total= sum(COUNT))
CHECK

test_that("Summarize recoveries", {
  recoveries_filter <- list(SPEC="ATBR", e.country_code='US', r.flyway=1)
  rec_db <- lincoln_filter_db(recoveries_filter, type="recoveries")
  pkg_sum <- summarize_recoveries(rec_db)
  colnames(pkg_sum) <- c("R.Month", "total")
  expect_equal(pkg_sum, CHECK)
})
