# constants
countries <- get_aux("countries")

# tests
test_that("get_stats() returns the correct format", {
  # Return table if simplify = TRUE
  df <- get_stats("AGO", year = 2000)
  expect_true(tibble::is_tibble(df))
  # Return custom response list if simplify = FALSE
  res <- get_stats("AGO", year = 2000, simplify = FALSE)
  expect_true(is.list(res))
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
})

test_that("get_stats() works a single country-year", {
  df <- get_stats("AGO", year = 2000)
  expect_equal(nrow(df), 1)
})

test_that("get_stats() works for all countries and years", {
  df <- get_stats("all", year = "all")
  skip("Looks like there is a potential data inconsitency in the API")
  expect_true(all(countries$country_code %in% df$country_code))
  expect_gte(nrow(df), 2000)
})

test_that("get_stats() works w/ fill_gaps = TRUE", {
  df <- get_stats("all", year = "all", fill_gaps = TRUE)
  expect_gte(nrow(df), 6000)
})

# test_that("get_stats() works w/ group_by = 'wb'", {
#   df <- get_stats("all", year = "all", group_by = 'wb')
# })
