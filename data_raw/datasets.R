## code to prepare `gamebirds` dataset goes here
# library(openxlsx)
rm(list=ls())
library(tidyverse)
source("R/prepare_data.R")
source("R/utils.R")
source("R/globals.R")

# https://www.pwrc.usgs.gov/BBL/manual/summary.cfm

# Generate replacement columns
# a <- colnames(gb_banding)
# names(a) <- colnames(gb_banding)
# dput(a)



gb_colnames <- read.csv('data_raw/colnames.csv')
gb_reporting_probas <- load_dataset('data_raw/reporting_probabilities.csv')
gb_ATBR_banding <- load_dataset("data_raw/ATBR_bandings.csv")
gb_ATBR_recoveries <- load_dataset("data_raw/ATBR_recoveries.csv")
gb_ATBR_harvest <- load_dataset("data_raw/ATBR_harvest.csv", stringsAsFactors = FALSE)


usethis::use_data(gb_ATBR_banding, overwrite = TRUE)
usethis::use_data(gb_ATBR_recoveries, overwrite = TRUE)
usethis::use_data(gb_ATBR_harvest, overwrite = TRUE)
usethis::use_data(gb_colnames, overwrite = TRUE)
usethis::use_data(gb_reporting_probas, overwrite = TRUE)



