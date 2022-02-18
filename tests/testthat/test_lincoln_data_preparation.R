source("globals_test.R")

test_that("Summarize banding", {
  filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  banding_db <- lincoln_filter_db(filter)
  pkg_sum <- get_banding_summary(banding_db)
  colnames(pkg_sum) <- c("B.Year", "total_banding")
  expect_equal(pkg_sum, ATBRBANDsum)
})



test_that("Summarize recoveries", {
  recoveries_filter <-
    list(
      SPEC = "ATBR",
      e.country_code = 'US',
      r.flyway = 1,
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


test_that("Get direct recoveries, ", {
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  rec_db <-
    lincoln_filter_db(recoveries_filter, type = "recoveries")
  pkg_sum = get_recoveries_summary(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})

test_that("Get direct recoveries, by bands", {
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  rec_db <-
    lincoln_filter_db(recoveries_filter, type = "recoveries")
  pkg_sum = get_recoveries_summary(rec_db, by_band = TRUE)
  colnames(pkg_sum) <- c("B.Year", "Add.Info", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOV_by_band_type)
})

test_that("Get total direct recoveries", {
  banding_filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  pkg_res <-
    get_all_direct_recoveries(banding_filter, recoveries_filter)
  colnames(pkg_res) <-
    c("B.Year", "total", "TAGE", "R.Corr.Year", "total_recoveries", "DRR")
  expect_equal(pkg_res, DRR)
})

test_that("Get HR, all bands", {
  banding_filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  pkg_res <-
    get_hr_df(banding_filter=banding_filter, recoveries_filter=recoveries_filter)
  colnames(pkg_res) <-
    c("B.Year", "total_banding", "R.Corr.Year", "total_recoveries", "DRR",
      "rho", "Var_rho", "SE_rho", "HR", "varDRR", "seDRR", "varh",
      "seh", "CL_h", "CV", "band_type")
  expect_equal(pkg_res, HR_all_band)
})


test_that("Get HR, no_geo bands", {
  banding_filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  pkg_res <-
    get_hr_df(banding_filter=banding_filter, recoveries_filter=recoveries_filter, band_type="no_geo")
  colnames(pkg_res) <-
    c("B.Year", "total", "R.Corr.Year", "total_recoveries", "DRR",
      "rho", "Var_rho", "SE_rho", "HR", "varDRR", "seDRR", "varh",
      "seh", "CL_h", "CV", "band_type")
  pkg_res <- pkg_res[pkg_res$B.Year %in% 2018:2019, ]
  rownames(pkg_res) <- 1:2
  expect_equal(pkg_res, HR_no_geo)
})

test_that("Use all bands", {
  banding_filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  recoveries_filter <-
    list(SPEC = "ATBR",
         e.country_code = 'US',
         r.flyway = 1)
  expect_equal(compare_band_types(banding_filter=banding_filter, recoveries_filter=recoveries_filter, plot=FALSE)$overlap, TRUE)
})

