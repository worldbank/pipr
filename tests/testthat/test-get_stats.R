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

test_that("get_stats() works for a single country-year", {
  df <- get_stats("AGO", year = 2000)
  expect_equal(nrow(df), 1)
})

test_that("get_stats() works for multiple countries and years", {
  df <- get_stats(c("AGO", "ALB"), year = 2000)
  expect_equal(nrow(df), 1)
  df <- get_stats(c("AGO", "ALB"), year = c(2000, 2018))
  expect_equal(nrow(df), 2)
})

test_that("get_stats() works for all countries and years", {
  df <- get_stats("all", year = "all")
  expect_gte(nrow(df), 2000)
  skip("Looks like there is a potential data inconsitency in the API")
  expect_true(all(countries$country_code %in% df$country_code))
})

test_that("get_stats() works w/ fill_gaps = TRUE", {
  df <- get_stats("all", year = "all", fill_gaps = TRUE)
  expect_gte(nrow(df), 6000)
})

test_that("get_stats() works w/ group_by = 'wb'", {
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE",
    message = "pip-grp not implement on PROD yet"
  )
  #  df <- get_stats("all", year = 2018, group_by = "wb")
  df <- get_stats("all", year = 2011, group_by = "wb", server = "qa")
  expect_equal(nrow(df), 8)
  expect_identical(
    df$region_code,
    c(
      "EAP", "ECA", "LAC", "MNA",
      "OHI", "SAS", "SSA", "WLD"
    )
  )
})

test_that("get_stats() works w/ group_by = 'none'", {
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE",
    message = "pip-grp not implement on PROD yet"
  )
  # df <- get_stats("all", year = 2018, group_by = "none")
  df <- get_stats("all", year = 2011, group_by = "none", server = "qa")
  expect_equal(nrow(df), 1)
  expect_identical(df$region_code, "CUSTOM")
  # df <- get_stats(c("ARG", "BRA"), year = 2011, group_by = "none")
  df <- get_stats(c("ARG", "BRA"), year = 2011, group_by = "none", server = "qa")
  expect_equal(nrow(df), 1)
  expect_identical(df$region_code, "CUSTOM")
})