source("globals_test.R")

test_that("Summarize banding", {
  filter <- list(SPEC = "ATBR", b.state_name = "Nunavut")
  banding_db <- lincoln_filter_db(filter)
  pkg_sum <- summarize_bandings(banding_db)
  colnames(pkg_sum) <- c("B.Year", "n_banded")
  expect_equal(pkg_sum, ATBRBANDsum)
})

filters <- list(SPEC = "ATBR", b.state_name = "Nunavut",e.country_code = 'US',
                r.flyway_code = 1)

test_that("Summarize recoveries", {
  recoveries_filter <-
    list(
      SPEC = "ATBR",
      e.country_code = 'US',
      r.flyway_code = 1,
      r.corrected_year = NULL
    )
  rec_db <-
    lincoln_filter_db(recoveries_filter, type = "recoveries")
  pkg_sum = rec_db %>%
    group_by(r.month) %>%
    summarise(total = n())
  colnames(pkg_sum) <- c("R.Month", "total_recoveries")
  expect_equal(pkg_sum, CHECK)
})


test_that("Get direct recoveries", {
  rec_db <-
    lincoln_filter_db(filters, type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})

test_that("Get direct recoveries, global filters ", {
  rec_db <-
    lincoln_filter_db(filters, type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})


test_that("Get direct recoveries, by bands", {
  rec_db <-
    lincoln_filter_db(filters, type = "recoveries")
  pkg_sum = summarize_recoveries(rec_db, by_band = TRUE)
  colnames(pkg_sum) <- c("B.Year", "Add.Info", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOV_by_band_type)
})

test_that("Get total direct recoveries", {
  pkg_res <-
    get_direct_recoveries(filters)
  colnames(pkg_res) <-
    c("B.Year", "n_banded", "TAGE", "R.Corr.Year", "total_recoveries", "DRR")
  expect_equal(pkg_res, DRR)
})

test_that("Get HR, all bands", {
  pkg_res <-
    get_hr_df(filters=filters)
  colnames(pkg_res) <-
    c("B.Year", "n_banded", "R.Corr.Year", "total_recoveries", "DRR",
      "rho", "Var_rho", "SE_rho", "HR", "varDRR", "seDRR", "varh",
      "seh", "CL_h", "CV", "band_type")
  expect_equal(pkg_res, HR_all_band)
})


test_that("Get HR, all bands, filters", {
  pkg_res <-
    get_hr_df(filters=filters)
  colnames(pkg_res) <-
    c("B.Year", "n_banded", "R.Corr.Year", "total_recoveries", "DRR",
      "rho", "Var_rho", "SE_rho", "HR", "varDRR", "seDRR", "varh",
      "seh", "CL_h", "CV", "band_type")
  expect_equal(pkg_res, HR_all_band)
})


test_that("Get HR, no_geo bands", {
  pkg_res <-
    get_hr_df(filters=filters, band_type="no_geo")
  colnames(pkg_res) <-
    c("B.Year", "n_banded", "R.Corr.Year", "total_recoveries", "DRR",
      "rho", "Var_rho", "SE_rho", "HR", "varDRR", "seDRR", "varh",
      "seh", "CL_h", "CV", "band_type")
  pkg_res <- pkg_res[pkg_res$B.Year %in% 2018:2019, ]
  rownames(pkg_res) <- 1:2
  expect_equal(pkg_res, HR_no_geo)
})



test_that("Use all bands", {
  expect_equal(compare_band_types(filters=filters, plot=FALSE)$overlap, TRUE)
})

