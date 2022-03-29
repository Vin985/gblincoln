rm(list = ls())

library(gblincoln)

##############
## Datasets ##
##############

# In-package database for banding data
gb_ATBR_banding

# In-package database for recovery data
gb_ATBR_recoveries

# In-package harvest data
gb_ATBR_harvest

# In-package reporting probabilities
gb_reporting_probas

# Conversion of Gamebirds names to R package
gb_colnames

###########################
## Get Lincoln estimates ##
###########################

# Define a filter for Lincoln estimates
filters_ATBR <-
  list(
    SPEC = "ATBR",
    b.state_name = "Nunavut",
    e.country_code = 'US',
    r.flyway_code = 1,
    b.year= 2000:2019,
    r.corrected_year=2000:2019
  )

# Directly get lincoln estimates using provided datasets
lincoln_estimates <-
  get_lincoln_estimates(
    filters = filters_ATBR,
    banding_df = gb_ATBR_banding,
    recoveries_df = gb_ATBR_recoveries,
    harvest_df = gb_ATBR_harvest,
    rho_df = gb_reporting_probas,
    harvest_correction_factor = 0.61
  )
lincoln_estimates

# Get the direct recoveries dataframe
dr_df <-
  get_direct_recoveries(banding_df = gb_ATBR_banding,
                        recoveries_df = gb_ATBR_recoveries,
                        filters = filters_ATBR)


# Get the harvest rate dataframe
hr_df <- get_harvest_rate(df = dr_df, rho_df = gb_reporting_probas)

# get the lincoln estimates using the previous dataframe
get_lincoln_estimates(df = hr_df, harvest_df = gb_ATBR_harvest)


#############################
## Harvest rate comparison ##
#############################

filters_ATBR_all <- filters_ATBR
# Create filter with no gelocators based on the one previously defined
filters_ATBR_nogeo <-
  list_update(filters_ATBR_all, list(add_info = c(00, 01, 07), b.year = 2018:2019))

# Compare harvest rates
res <-
  compare_harvest_rates(
    filters1 = filters_ATBR_all,
    filters2 = filters_ATBR_nogeo,
    banding_df = gb_ATBR_banding,
    recoveries_df = gb_ATBR_recoveries,
    rho_df = gb_reporting_probas
  )
# res checks if the confidence levels of the harvest rates overlap for all values
res # All confidence levels overlap, so there should not be a problem to use geolocators

###############################
## Get Location informations ##
###############################

get_species_locations(gb_ATBR_banding, "ATBR", sort_by = "b.country_name")

get_species_countries(gb_ATBR_banding, "ATBR", return_codes=FALSE)
get_species_flyways(gb_ATBR_recoveries, "ATBR")
get_species_states(gb_ATBR_banding, "ATBR", return_names = FALSE)
