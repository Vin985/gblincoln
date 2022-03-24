source("globals_test.R")


filters <-
  list(
    SPEC = "ATBR",
    b.state_name = "Nunavut",
    e.country_code = 'US',
    r.flyway_code = 1,
    b.year= 2000:2019,
    r.corrected_year=2000:2019
  )
filters_no_geo <-
  list_update(filters, list(add_info = c(00, 01, 07)))


test_that("Add specific_filter", {
  spec_filter <- list_update(filters, list(banding_filters=list(b.year=2010:2019)))
  spec_filter["r.corrected_year"] <- NULL
  expect_equal(add_db_filters(spec_filter, "b"),
                 list(
                   SPEC = "ATBR",
                   b.state_name = "Nunavut",
                   e.country_code = 'US',
                   r.flyway_code = 1,
                   b.year= 2010:2019))
})


test_that("Summarize banding", {
  banding_db <- filter_database(gb_ATBR_banding, filters, db_type="b")
  pkg_sum <- summarize_bandings(banding_db)
  colnames(pkg_sum) <- c("B.Year", "n_banded")
  expect_equal(pkg_sum, ATBRBANDsum)
})

# test_that("Summarize recoveries", {
#   rec_db <-
#     filter_database(gb_ATBR_recoveries, filters)
#   pkg_sum = summarize_recoveries(rec_db)
#   colnames(pkg_sum) <- c("R.Month", "total_recoveries")
#   expect_equal(pkg_sum, CHECK)
# })


test_that("Get direct recoveries", {
  rec_db <-
    filter_database(gb_ATBR_recoveries, filters, db_type="r")
  pkg_sum = summarize_recoveries(rec_db)
  colnames(pkg_sum) <-
    c("TAGE", "B.Year", "R.Corr.Year", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOVsum)
})


test_that("Get direct recoveries, by bands", {
  rec_db <-
    filter_database(gb_ATBR_recoveries, filters, db_type="r")
  pkg_sum = summarize_recoveries(rec_db, by_band = TRUE)
  colnames(pkg_sum) <- c("B.Year", "Add.Info", "total_recoveries")
  expect_equal(pkg_sum, ATBRRECOV_by_band_type)
})

test_that("Get total direct recoveries", {
  pkg_res <-
    get_direct_recoveries(gb_ATBR_banding, gb_ATBR_recoveries, filters)
  pkg_res <- pkg_res[,-(7:8)]
  colnames(pkg_res) <-
    c("B.Year",
      "n_banded",
      "TAGE",
      "R.Corr.Year",
      "total_recoveries",
      "DRR")
  expect_equal(pkg_res, DRR)
})

test_that("Get harvest rate, all bands", {
  pkg_res <-
    get_harvest_rate(filters = filters,
                     banding_df = gb_ATBR_banding,
                     recoveries_df = gb_ATBR_recoveries)
  colnames(pkg_res) <-
    c(
      "B.Year",
      "n_banded",
      "TAGE",
      "R.Corr.Year",
      "total_recoveries",
      "DRR",
      "varDRR",
      "seDRR",
      "rho",
      "Var_rho",
      "SE_rho",
      "HR",
      "varh",
      "seh",
      "CL_h",
      "CV"
    )
  expect_equal(pkg_res[, sort(colnames(pkg_res))], HR_all_band[, sort(colnames(HR_all_band))])
})


test_that("Get HR, no_geo bands", {
  pkg_res <-
    get_harvest_rate(filters = filters_no_geo,
                     banding_df = gb_ATBR_banding,
                     recoveries_df = gb_ATBR_recoveries)
  colnames(pkg_res) <- c(
    "B.Year",
    "n_banded",
    "TAGE",
    "R.Corr.Year",
    "total_recoveries",
    "DRR",
    "varDRR",
    "seDRR",
    "rho",
    "Var_rho",
    "SE_rho",
    "HR",
    "varh",
    "seh",
    "CL_h",
    "CV"
  )
  pkg_res <- pkg_res[pkg_res$B.Year %in% 2018:2019,]
  rownames(pkg_res) <- 1:2
  expect_equal(pkg_res[, sort(colnames(pkg_res))], HR_no_geo[, sort(colnames(HR_no_geo))])
})



test_that("Use all bands", {
  expect_equal(
    compare_harvest_rates(
      filters1 = filters,
      filters2 = filters_no_geo,
      banding_df = gb_ATBR_banding,
      recoveries_df = gb_ATBR_recoveries,
      plot = FALSE
    ),
    TRUE
  )
})
