source("globals_test.R")

test_that("Summarize banding", {
  filter <- list(SPEC = "ATBR", state_name = "Nunavut")
  banding_db <- lincoln_filter_db(filter)
  pkg_sum <- get_banding_summary(banding_db)
  colnames(pkg_sum) <- c("B.Year", "total")
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
  colnames(pkg_sum) <- c("R.Month", "total")
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
    c("B.Year", "total", "TAGE", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_res, DIRECT_RECOVS)
})
