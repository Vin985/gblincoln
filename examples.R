rm(list=ls())

library(gblincoln)

# In-package database for banding data
gb_banding
# In-package database for recovery data
gb_recoveries

# Conversion of Gamebirds names to R package
gb_colnames

# Define a filter for Lincoln estimates
filters <- list(SPEC = "ATBR", b.state_name = "Nunavut",e.country_code = 'US',
                r.flyway_code = 1)


# Filter database to prepare for Lincoln estimates. By default, use banding dataset
# and all bands
filter_database(gb_banding, filters)
# Same thing, different way to call it
filter_database(gb_banding, filters, db_type="b", band_type = "all")

# Recoveries
filter_database(gb_recoveries, filters, type="recoveries")
# same thing, shortcut
filter_database(gb_recoveries,filters, type="r")
# No gelocators
filters_no_geo <- list_update(filters, list(add_info = c(00, 01, 07)))
filter_database(gb_recoveries, filters_no_geo, type="recoveries")

# Get all direct recoveries dataframe. Actually call the filter function on
# Banding and recoveries dataframe
direct_recoveries <- get_direct_recoveries(filters=filters)

# hr dataframe (uses the direct recoveries dataframe and apply RHO reporting
# probabilities)
hr_all_bands <- get_hr_df(filters=filters)
# Same thing using a direct recoveries data frame instead
hr_all_bands2 <- get_hr_df(drr_df = direct_recoveries)

# Same with No geolocator
hr_no_geo <- get_hr_df(filters=filters, band_type="no_geo")


# Alternatively, select directly the best dataframe based on confidence levels comparison
comparison <- compare_band_types(filters=filters)
print(comparison)
# Best dataframe is selected
hr_df <- comparison$hr_df

# Load harvest data
ATBRharvest <-
  read.csv("/mnt/win/dev/EC/Lincoln/data/ATBR_harvest_2000_2019(atlantic flyway).csv",
           stringsAsFactors = FALSE)
colnames(ATBRharvest)[1] <- "b.year"

# Get Lincoln estimates for the data
get_lincoln_estimates(filters=filters, hr_df=hr_all_bands, harvest_df=ATBRharvest)


# Perform multiple estimates for different filters
# NOT YET FULLY FUNCTIONAL! NEEDS HARVEST DATA FOR ALL SPECIES
filters_list <- list(list(SPEC = "ATBR", b.state_name = "Nunavut",e.country_code = 'US',
                          r.flyway_code = 1),
                     list(SPEC = "ATBR", b.state_name = "Nunavut",e.country_code = 'US',
                          r.flyway_code = 1))

lincoln_estimates(filters_list, harvest_df=ATBRharvest)
