## code to prepare `gamebirds` dataset goes here
# library(openxlsx)
rm(list=ls())
library(tidyverse)
source("R/prepare_datasets.R")
source("R/globals.R")

# https://www.pwrc.usgs.gov/BBL/manual/summary.cfm

# Generate replacement columns
# a <- colnames(gb_banding)
# names(a) <- colnames(gb_banding)
# dput(a)



gb_colnames <- read.csv('data_raw/colnames.csv')
gb_reporting_probas <- read.csv('data_raw/reporting_probabilities.csv')

# gb_banding <- read.xlsx("data_raw/gamebirds_banding.xlsx")
gb_banding <- read.csv("data_raw/gamebirds_banding.csv")
gb_banding <- clean_dataset(gb_banding)

gb_recoveries <- read.csv("data_raw/gamebirds_recoveries.csv")
gb_recoveries <- clean_dataset(gb_recoveries, recoveries=TRUE)



usethis::use_data(gb_banding, overwrite = TRUE)
usethis::use_data(gb_recoveries, overwrite = TRUE)
usethis::use_data(gb_colnames, overwrite = TRUE)
usethis::use_data(gb_reporting_probas, overwrite = TRUE)



