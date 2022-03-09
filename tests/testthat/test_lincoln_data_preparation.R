source("globals_test.R")

test_that("Summarize banding", {
  filter <- list(SPEC = "ATBR", b.state_name = "Nunavut")
  banding_db <- filter_database(gb_banding, filter)
  pkg_sum <- summarize_bandings(banding_db)
  colnames(pkg_sum) <- c("B.Year", "n_banded")
  expect_equal(pkg_sum, ATBRBANDsum)
})

filters <-
  list(
    SPEC = "ATBR",
    b.state_name = "Nunavut",
    e.country_code = 'US',
    r.flyway_code = 1
  )
filters_no_geo <-
  list_update(filters, list(add_info = c(00, 01, 07)))

test_that("Summarize recoveries", {
  recoveries_filter <-
    list(
      SPEC = "ATBR",
      e.country_code = 'US',
      r.flyway_code = 1,
      r.corrected_year = NULL
    )
  rec_db <-
    filter_database(gb_recoveries, recoveries_filter, db_type = "recoveries")
  pkg_sum = rec_db %>%
    group_by(r.month) %>%
    summarise(total = n())
  colnames(pkg_sum) <- c("R.Month", "total_recoveries")
  expect_equal(pkg_sum, CHECK)
})


test_that("Get direct recoveries", {
  rec_db <-
    filter_database(gb_recoveries, filters, db_type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})

test_that("Get direct recoveries, global filters ", {
  rec_db <-
    filter_database(gb_recoveries, filters, db_type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})


test_that("Get direct recoveries, by bands", {
  rec_db <-
    filter_database(gb_recoveries, filters, db_type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db, by_band = TRUE)
  colnames(pkg_sum) <- c("B.Year", "Add.Info", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOV_by_band_type)
})

test_that("Get total direct recoveries", {
  pkg_res <-
    get_direct_recoveries(filters, gb_banding, gb_recoveries)
  colnames(pkg_res) <-
    c("B.Year",
      "n_banded",
      "TAGE",
      "R.Corr.Year",
      "total_recoveries",
      "DRR")
  expect_equal(pkg_res, DRR)
})

test_that("Get HR, all bands", {
  pkg_res <-
    get_hr_df(filters = filters,
              banding_df = gb_banding,
              recoveries_df = gb_recoveries)
  colnames(pkg_res) <-
    c(
      "B.Year",
      "n_banded",
      "R.Corr.Year",
      "total_recoveries",
      "DRR",
      "rho",
      "Var_rho",
      "SE_rho",
      "HR",
      "varDRR",
      "seDRR",
      "varh",
      "seh",
      "CL_h",
      "CV",
      "band_type"
    )
  expect_equal(pkg_res, HR_all_band)
})


test_that("Get HR, all bands, filters", {
  pkg_res <-
    get_hr_df(filters = filters,
              banding_df = gb_banding,
              recoveries_df = gb_recoveries)
  colnames(pkg_res) <-
    c(
      "B.Year",
      "n_banded",
      "R.Corr.Year",
      "total_recoveries",
      "DRR",
      "rho",
      "Var_rho",
      "SE_rho",
      "HR",
      "varDRR",
      "seDRR",
      "varh",
      "seh",
      "CL_h",
      "CV",
      "band_type"
    )
  expect_equal(pkg_res, HR_all_band)
})


test_that("Get HR, no_geo bands", {
  pkg_res <-
    get_hr_df(filters = filters_no_geo,
              ,
              banding_df = gb_banding,
              recoveries_df = gb_recoveries)
  colnames(pkg_res) <-
    c(
      "B.Year",
      "n_banded",
      "R.Corr.Year",
      "total_recoveries",
      "DRR",
      "rho",
      "Var_rho",
      "SE_rho",
      "HR",
      "varDRR",
      "seDRR",
      "varh",
      "seh",
      "CL_h",
      "CV",
      "band_type"
    )
  pkg_res <- pkg_res[pkg_res$B.Year %in% 2018:2019,]
  rownames(pkg_res) <- 1:2
  expect_equal(pkg_res, HR_no_geo)
})



test_that("Use all bands", {
  expect_equal(compare_filters(filters = filters, plot = FALSE)$overlap,
               TRUE)
})
