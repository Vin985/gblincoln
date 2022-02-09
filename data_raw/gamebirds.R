## code to prepare `gamebirds` dataset goes here
# library(openxlsx)
rm(list=ls())
library(tidyverse)
source("R/globals.R")

# https://www.pwrc.usgs.gov/BBL/manual/summary.cfm


# Generate replacement columns
# a <- colnames(gb_banding)
# names(a) <- colnames(gb_banding)
# dput(a)

rename_columns <- function(df, columns){
  found_columns <- match(colnames(df), columns$old_colnames)
  new_names <- columns[found_columns, "new_colnames"]
  new_columns <- colnames(df)
  new_columns[!is.na(new_names)] <- new_names[!is.na(new_names)]
  colnames(df) <- new_columns
  return(df)
}

set_age_classes <- function(df, age_classes = NULL) {
  if (is.null(age_classes)) {
    age_classes <- AGE_CLASSES
  }
  i = 1
  for (age_class in age_classes) {
    df$age_short[df$age_code %in% age_class] <- names(age_classes)[i]
    i <- i + 1
  }
  df$age_short[df$age_code == 0] <- NA

  return(df)
}


set_sex_classes <- function(df, sex_classes = NULL) {
  if(is.null(sex_classes)){
    sex_classes <- SEX_CLASSES
  }
  i = 1
  for (sex_class in sex_classes) {
    df$sex[df$sex_code %in% sex_class] <- names(sex_classes)[i]
    i <- i + 1
  }
  return(df)
}

clean_dataset <- function(df, colnames = NULL) {
  if(is.null(colnames)) {
    colnames <- GB_COLNAMES
  }

  cleaned <-
    df %>% rename_columns(colnames) %>% set_age_classes() %>% set_sex_classes()

  return(cleaned)
}

# gb_banding <- read.xlsx("data_raw/gamebirds_banding.xlsx")
gb_banding <- read.csv("data_raw/gamebirds_banding.csv")
gb_banding <- clean_dataset(gb_banding)

gb_recoveries <- read.csv("data_raw/gamebirds_recoveries.csv")
gb_recoveries <- clean_dataset(gb_recoveries)


usethis::use_data(gb_banding, overwrite = TRUE)
usethis::use_data(gb_recoveries, overwrite = TRUE)
