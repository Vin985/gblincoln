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


ATBRRECOV <-
  read.csv("/mnt/win/dev/EC/Lincoln/data/ATBR_allrecovs.csv",
           stringsAsFactors = FALSE)
names(ATBRRECOV) <-
  str_replace_all(names(ATBRRECOV), c(" " = "." , "," = ""))

ATBRRECOV$TAGE[ATBRRECOV$Age == 1] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 5] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 6] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 7] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 8] <- "AHY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 2] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 3] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 4] <- "HY"
ATBRRECOV$TAGE[ATBRRECOV$Age == 0] <- NA
ATBRRECOV$TSEX[ATBRRECOV$Sex == 4] <- "MALE"
ATBRRECOV$TSEX[ATBRRECOV$Sex == 5] <- "FEMALE"
ATBRRECOV$COUNT <- 1

ATBRRECOV1 <- ATBRRECOV %>%
  filter(Status == 3) %>% # normal, wild birds
  filter(Add.Info %in% c(00, 01, 07, 08, 25, 18),  #00 (normal), 01 (color band), 07 (double bands), 08 (temp marker-paint or dye), 25 (geolocators), 18 (blood sampled)
         Sex %in% c(4, 5)) %>%
  filter(TAGE == 'AHY') %>%
  filter(How.Obt == 1) %>%
  filter(e_country_code == 'US') %>%
  filter(R.Flyway == 1) %>%
  rename(LOC = Location_lu..STATE_NAME)

ATBRRECOV2 <- ATBRRECOV1 %>%
  filter(!is.na(TAGE)) %>%
  filter(LOC %in% c("Nunavut")) %>%
  filter(R.Month %in% c(9, 10, 11, 12, 1, 2, 3)) %>%
  filter(B.Month %in% c(7, 8)) %>%
  select(
    -B.Coordinate.Precision,-b_country_code,-b_state_code,-Band.Size,-Band.Type.Current,-Band.Type.Orig,
    -Cardinal.Direction,-Distance,-e_state_code,-How.Aged,-How.Obt,-How.Sexed,-Hunt..Season.Surv.,
    -Marker_Desc_bndg,-Marker_Desc_enc,-MIN_AGE_AT_ENC,-other_bands,-Permit,-Pres..Cond.,-R.Coordinate.Precision,
    -R.Create.date.Month,-R.Create.date.Year,-R.Dir,-Replaced.Band.Code,-Replaced.Band.Translated,-Same.Block,
    -Who.Reported,-Why..pre.1994...Report.Method..after.1994.,-Age..VAGE,-AI..VAI,-B.Coord.Precision..LOCATION_ACCURACY_DESC,-BDay..VRDay,
    -BMonth..VMonth,-BRegion..STA,-BRegion..State,-BType.Current..VBtype,-BType.Current..VText,-BType..VBtype,-BType..VText,
    -Condition..VBandStatus,-Condition..VCondition,-How_aged..How.Aged.Description,
    -How_sexed..How.Sexed.Description,-Location_lu_enc..STATE_NAME,-Location_lu..COUNTRY_NAME,
    -Permits..Permittee,-R.Coord.Precision..LOCATION_ACCURACY_DESC,-RDay..VRDay,-Region..Flyway,-How.Obt..VHow,
    -Rept.Mthd..VRept.Mthd,-RMonth..VMonth,-Sex..VSEX,-Species.Game.Birds..SPEC,-Species.Game.Birds..Species,-Status..VStatus,
    -Who..VWho
  )

CHECK <- ATBRRECOV2 %>%
  group_by(R.Month) %>%
  summarise(total = sum(COUNT))
CHECK

ATBRRECOV2$R.Corr.Year <-
  ifelse(ATBRRECOV2$R.Month < 4, ATBRRECOV2$R.Year - 1, ATBRRECOV2$R.Year)

ATBRRECOV3 <- ATBRRECOV2 %>%
  filter(R.Corr.Year %in% 2000:2019)

ATBRRECOVsum <- ATBRRECOV3 %>%
  filter(B.Year == R.Corr.Year) %>%
  group_by(TAGE, B.Year, R.Corr.Year) %>%
  summarise(total_recoveries = sum(COUNT))

ATBRRECOV_by_band_type <- ATBRRECOV3 %>%
  filter(B.Year == R.Corr.Year) %>%
  group_by(B.Year, Add.Info) %>%
  summarise(total_recoveries = sum(COUNT))

DIRECT_RECOVS <- merge(ATBRBANDsum, ATBRRECOVsum)
DRR <- DIRECT_RECOVS %>%
  mutate(DRR = total_recoveries / total)


RHO <-
  read.csv(
    "/mnt/win/dev/EC/Lincoln/data/RHO_1976_2010(Arnold2020)_2011_2019(linear).csv",
    stringsAsFactors = FALSE
  )


HR <- inner_join(DRR, RHO, by = "B.Year")
HR_all_band <- HR %>%
  mutate(HR = DRR / rho) %>%
  mutate(varDRR = (DRR * (1 - DRR)) / (total - 1)) %>%
  mutate(seDRR = sqrt(varDRR)) %>%
  mutate(varh = (varDRR / (rho ^ 2)) + ((DRR ^ 2 * Var_rho) / rho ^ 4)) %>%
  mutate(seh = sqrt(varh)) %>%
  mutate(CL_h = seh * 1.96) %>%
  mutate(CV = seh / HR) %>%
  mutate(band_type = 'all') %>%
  select(-TAGE)


ATBRBAND_no_geo <- ATBRBAND2 %>%
  filter(B.Year %in% 2000:2019) %>%
  filter(Add.Info %in% c(00, 01, 07)) %>%
  group_by(B.Year) %>%
  summarise(total = sum(Count.of.Birds))
ATBRBAND_no_geo

ATBRRECOV_no_geo <- ATBRRECOV3 %>%
  filter(B.Year == R.Corr.Year) %>%
  filter(Add.Info %in% c(00, 01, 07)) %>%
  group_by(TAGE, B.Year, R.Corr.Year) %>%
  summarise(total_recoveries = sum(COUNT))


DIRECT_RECOVS_no_geo <- merge(ATBRBAND_no_geo, ATBRRECOV_no_geo) %>%
  mutate(DRR = total_recoveries / total)

HR_no_geo <- inner_join(DIRECT_RECOVS_no_geo, RHO, by = "B.Year") %>%
  mutate(HR = DRR / rho) %>%
  mutate(varDRR = (DRR * (1 - DRR)) / (total - 1)) %>%
  mutate(seDRR = sqrt(varDRR)) %>%
  mutate(varh = (varDRR / (rho ^ 2)) + ((DRR ^ 2 * Var_rho) / rho ^ 4)) %>%
  mutate(seh = sqrt(varh)) %>%
  mutate(CL_h = seh * 1.96) %>%
  mutate(CV = seh / HR) %>%
  mutate(band_type = 'no_geo') %>%
  filter(B.Year %in% 2018:2019) %>%
  select(-TAGE)

# HR_by_band_type <- rbind(HR_all_band, HR_no_geo)
