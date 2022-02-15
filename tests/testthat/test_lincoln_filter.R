
test_that("Time filter, no start", {
  filter <- list(b.month = c(from = 09, to = 10))
  expect_error(lincoln_filter_db(filter))
})

test_that("Time filter, no end", {
  filter <- list(b.month = c(start = 09, to = 10))
  expect_error(lincoln_filter_db(filter))
})

test_that("Filter, wrong type", {
  expect_error(lincoln_filter_db(type="s"))
})


test_that("Filter column names", {
  filter1 <- list(Add.Info="00")
  filter2 <- list(add_info="00")
  expect_equal(lincoln_filter_db(filter1), lincoln_filter_db(filter2))
})


test_that("Filter column names duplicate", {
  filter1 <- list(add_info="00", Add.Info="01")
  filter2 <- list(add_info="01")
  expect_equal(lincoln_filter_db(filter1), lincoln_filter_db(filter2))
})
