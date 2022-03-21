rm(list = ls())

library(gblincoln)

# In-package database for banding data
gb_banding
# In-package database for recovery data
gb_recoveries

# Conversion of Gamebirds names to R package
gb_colnames

# Define a filter for Lincoln estimates
filters <-
  list(
    SPEC = "ATBR",
    b.state_name = "Nunavut",
    e.country_code = 'US',
    r.flyway_code = 1
  )


# Filter database to prepare for Lincoln estimates. By default, use banding dataset
# and all bands
filter_database(gb_banding, filters)
# Same thing, different way to call it
filter_database(gb_banding, filters, db_type = "b")

# Recoveries
filter_database(gb_recoveries, filters, db_type = "recoveries")
# same thing, shortcut
filter_database(gb_recoveries, filters, db_type = "r")
# No gelocators
filters_no_geo <-
  list_update(filters, list(add_info = c(00, 01, 07), b.year = 2018:2019))
filter_database(gb_recoveries, filters_no_geo, db_type = "recoveries")

# Get all direct recoveries dataframe. Actually call the filter function on
# Banding and recoveries dataframe
direct_recoveries <-
  get_direct_recoveries(filters = filters,
                        banding_df = gb_banding,
                        recoveries_df = gb_recoveries)

# hr dataframe (uses the direct recoveries dataframe and apply RHO reporting
# probabilities)
hr_all_bands <-
  get_harvest_rate(
    filters = filters,
    banding_df = gb_banding,
    recoveries_df = gb_recoveries,
    rho_df = gb_reporting_probas
  )
# Same thing using a direct recoveries data frame instead
hr_all_bands2 <-
  get_harvest_rate(direct_recoveries, rho_df = gb_reporting_probas)

# Same with No geolocator
hr_no_geo <-
  get_harvest_rate(
    filters = filters_no_geo,
    banding_df = gb_banding,
    recoveries_df = gb_recoveries,
    rho_df = gb_reporting_probas
  )


# Alternatively, select directly the best dataframe based on confidence levels comparison
comparison <- compare_harvest_rates(
  filters1 = filters,
  filters2 = filters_no_geo,
  banding_df = gb_banding,
  recoveries_df = gb_recoveries,
)
print(comparison)
# Best dataframe is selected
hr_df <- hr_all_bands

# Load harvest data
ATBRharvest <-
  read.csv(
    "/mnt/win/dev/EC/Lincoln/data/ATBR_harvest_2000_2019(atlantic flyway).csv",
    stringsAsFactors = FALSE
  )
colnames(ATBRharvest)[1] <- "b.year"

# Get Lincoln estimates for the data
get_lincoln_estimates(hr_all_bands, harvest_df = ATBR_harvest)

get_lincoln_estimates(
  harvest_df = ATBRharvest,
  filters = filters,
  banding_df = gb_banding,
  recoveries_df = gb_recoveries,
  rho_df = gb_reporting_probas
)
