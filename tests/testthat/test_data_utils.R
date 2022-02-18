REF_COUNTRIES <- c("", "Canada", "United States")
REF_FLYWAYS <- c("Alaska", "Atlantic", "Canada", "Pacific")
REF_STATES <-
  c(
    "Alaska",
    "California",
    "Connecticut",
    "Keewatin",
    "Maine",
    "Maryland",
    "Massachusetts",
    "New Brunswick",
    "New Jersey",
    "New York",
    "Northwest Territories",
    "Nova Scotia",
    "Nunavut",
    "Ontario",
    "Rhode Island",
    "Virginia",
    "Washington"
  )

REF_LOCATIONS <-
  structure(
    list(
      b.country_name = c(
        "United States",
        "Canada",
        "United States",
        "United States",
        "Canada",
        "United States",
        "United States",
        "United States",
        "United States",
        "United States",
        "United States",
        "United States",
        "Canada",
        "Canada",
        "Canada",
        "",
        "",
        "",
        "",
        ""
      ),
      b.state_name = c(
        "New York",
        "Nunavut",
        "New Jersey",
        "Virginia",
        "Northwest Territories",
        "Massachusetts",
        "Connecticut",
        "Maryland",
        "Rhode Island",
        "Maine",
        "Alaska",
        "Washington",
        "Ontario",
        "Nova Scotia",
        "New Brunswick",
        "Maine",
        "New Jersey",
        "New York",
        "California",
        "Keewatin"
      ),
      b.flyway_name = c(
        "Atlantic",
        "Canada",
        "Atlantic",
        "Atlantic",
        "Canada",
        "Atlantic",
        "Atlantic",
        "Atlantic",
        "Atlantic",
        "Atlantic",
        "Alaska",
        "Pacific",
        "Canada",
        "Canada",
        "Canada",
        "Atlantic",
        "Atlantic",
        "Atlantic",
        "Pacific",
        "Canada"
      )
    ),
    row.names = c(NA, -20L),
    class = "data.frame"
  )

test_that("Species countries", {
  expect_equal(get_species_countries("ATBR"), REF_COUNTRIES)
})

test_that("Species flyways", {
  expect_equal(get_species_flyways("ATBR"), REF_FLYWAYS)
})

test_that("Species states", {
  expect_equal(get_species_states("ATBR"), REF_STATES)
})

test_that("Species states manual", {
  expect_equal(get_species_locations("ATBR", "b.state_name"), REF_STATES)
})


test_that("Species locations", {
  expect_equal(get_species_locations("ATBR"), REF_LOCATIONS)
})


test_that("Species locations sort", {
  ref <- arrange(REF_LOCATIONS, b.state_name)
  expect_equal(get_species_locations("ATBR", sort_by = "b.state_name"), ref)
})

test_that("Species locations sort, 2 cols", {
  ref <- unique(REF_LOCATIONS[, c("b.flyway_name", "b.country_name")])
  ref <- arrange(ref, b.country_name)
  expect_equal(get_species_locations("ATBR", c("b.flyway_name", "b.country_name"), sort_by = "b.country_name"),
               ref)
})

test_that("Missing column", {
  expect_equal(get_species_locations("ATBR", columns = c("no_col")), NULL)
})

test_that("Check type r", {
  expect_equal(check_type("rECOVERIEs"), "r")
})

test_that("Check type r2", {
  expect_equal(check_type("R"), "r")
})

test_that("Check type b", {
  expect_equal(check_type("b"), "b")
})


test_that("Check type b2", {
  expect_equal(check_type("bANdinG"), "b")
})

test_that("Check type error", {
  expect_error(check_type("bANdin"))
})

test_that("Get database", {
  expect_equal(get_db("bANdinG"), gb_banding)
})

test_that("Check columns", {
  expect_equal(check_columns(c("Add.Info", "add_info")), c("add_info", "add_info"))
})
