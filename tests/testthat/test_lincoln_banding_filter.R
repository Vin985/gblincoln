

# test_that("Default gb_banding filtering", {
#   expected_default <- read.csv("filter_default.csv")
#   test <- lincoln_banding_filter()
#   rownames(test) <- 1:nrow(test)
#   expect_equal(test, expected_default)
# })


test_that("Time filter, no start", {
  filter <- list(b.month_code = c(from = 09, to = 10))
  expect_error(lincoln_filter_db(filter))
})

test_that("Time filter, no end", {
  filter <- list(b.month_code = c(start = 09, to = 10))
  expect_error(lincoln_filter_db(filter))
})

test_that("Filter, wrong type", {
  expect_error(lincoln_filter_db(type="s"))
})

