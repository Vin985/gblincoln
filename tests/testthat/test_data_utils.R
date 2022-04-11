REF_COUNTRIES <- c("", "Canada", "United States")
REF_FLYWAYS <-
  structure(
    list(
      b.flyway_name = c("Atlantic", "Canada", "Alaska",
                        "Pacific"),
      b.flyway_code = c(1L, 6L, 5L, 4L)
    ),
    row.names = c(NA,-4L),
    class = "data.frame"
  )
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

REF_STATES_CODES <-
  c(2L, 4L, 7L, 8L, 9L, 13L, 14L, 23L, 24L, 25L, 34L, 36L, 44L,
    51L, 53L)

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
    row.names = c(NA,-20L),
    class = "data.frame"
  )

REF_LOCATIONS_ALL <-
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
      b.country_code = c(
        "US",
        "CA",
        "US",
        "US",
        "CA",
        "US",
        "US",
        "US",
        "US",
        "US",
        "US",
        "US",
        "CA",
        "CA",
        "CA",
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
      b.state_code = c(
        36L,
        14L,
        34L,
        51L,
        13L,
        25L,
        9L,
        24L,
        44L,
        23L,
        2L,
        53L,
        8L,
        7L,
        4L,
        NA,
        NA,
        NA,
        NA,
        NA
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
      ),
      b.flyway_code = c(
        1L,
        6L,
        1L,
        1L,
        6L,
        1L,
        1L,
        1L,
        1L,
        1L,
        5L,
        4L,
        6L,
        6L,
        6L,
        1L,
        1L,
        1L,
        4L,
        6L
      )
    ),
    row.names = c(NA,-20L),
    class = "data.frame"
  )

test_that("Species countries", {
  expect_equal(
    get_species_countries(gb_ATBR_banding, "ATBR", return_codes = FALSE),
    REF_COUNTRIES
  )
})

test_that("Species flyways", {
  expect_equal(get_species_flyways(gb_ATBR_banding, "ATBR"), REF_FLYWAYS)
})

test_that("Species states codes only", {
  expect_equal(
    get_species_states(gb_ATBR_banding, "ATBR", return_names = FALSE),
    REF_STATES_CODES
  )
})

test_that("Species states manual", {
  expect_equal(
    get_species_locations(gb_ATBR_banding, "ATBR", columns = "b.state_name"),
    REF_STATES
  )
})


test_that("Species locations", {
  expect_equal(get_species_locations(gb_ATBR_banding, "ATBR"),
               REF_LOCATIONS_ALL)
})


test_that("Species locations sort", {
  ref <- arrange(REF_LOCATIONS, b.state_name)
  expect_equal(
    get_species_locations(
      gb_ATBR_banding,
      "ATBR",
      return_codes = FALSE,
      sort_by = "b.state_name"
    ),
    ref
  )
})

test_that("Species locations sort, 2 cols", {
  ref <- unique(REF_LOCATIONS[, c("b.flyway_name", "b.country_name")])
  ref <- arrange(ref, b.country_name)
  expect_equal(
    get_species_locations(
      gb_ATBR_banding,
      "ATBR",
      columns = c("b.flyway_name", "b.country_name"),
      sort_by = "b.country_name"
    ),
    ref
  )
})

test_that("Missing column", {
  expect_equal(get_species_locations(gb_ATBR_banding, "ATBR", columns = c("no_col")),
               NULL)
})

test_that("Check columns", {
  expect_equal(check_columns(c("Add.Info", "add_info")), c("add_info", "add_info"))
})

test_that("Get database type recoveries", {
  expect_equal(get_db_type("r"), "recoveries")
})

test_that("Get database type banding", {
  expect_equal(get_db_type("banding"), "banding")
})
test_that("Get database type harvest", {
  expect_equal(get_db_type("HarVest"), "harvest")
})
test_that("Get database type rho", {
  expect_equal(get_db_type("RHO"), "rho")
})
