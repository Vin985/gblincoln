

# test_that("Default gb_banding filtering", {
#   expected_default <- read.csv("filter_default.csv")
#   test <- lincoln_banding_filter()
#   rownames(test) <- 1:nrow(test)
#   expect_equal(test, expected_default)
# })


test_that("Time filter, no start", {
  filter <- list(month = c(from = 09, to = 10))
  expect_error(lincoln_banding_filter(filter))
})

test_that("Time filter, no end", {
  filter <- list(month = c(start = 09, to = 10))
  expect_error(lincoln_banding_filter(filter))
})

