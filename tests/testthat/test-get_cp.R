library(testthat)
library(pipr)

# 1. General Argument Matching Tests ----
test_that("Argument matching works correctly for get_cp()", {
  skip_if_offline()
  skip_on_cran()
  # Default arguments, one country, simplify = TRUE
  res <- get_cp(country = "AGO")
  expect_type(res, "list") # Default simplify = TRUE returns a data.frame

  # Default arguments, one country, simplify = FALSE
  res <- get_cp(country = "AGO", simplify = FALSE)
  expect_s3_class(res, "pip_api")

  # Argument matching for 'format'
  expect_error(get_cp(format = "txt"), "'arg' should be one of")

  # Argument matching for 'api_version'
  expect_error(get_cp(api_version = "v2"), "'arg' should be")

})

# 2. povline Set-up Tests ----
test_that("povline and ppp_version arguments work correctly", {
  skip_if_offline()
  skip_on_cran()
  # Default povline
  res <- get_cp(country = "AGO")
  expect_true(any(res$poverty_line == 2.15))

  res <- get_cp(country = "AGO", ppp_version = 2017)
  expect_true(any(res$poverty_line == 2.15))

  # povline with ppp_version 2011
  res <- get_cp(country = "AGO", ppp_version = 2011, povline = NULL)
  expect_true(any(res$poverty_line == 1.9))

  # povline when povline is provided
  res <- get_cp(country = "AGO", povline = 3.2)
  expect_true(any(res$poverty_line == 3.2))
})


# 3. Other Tests ----
test_that("Requests execute successfully for get_cp()", {
  skip_if_offline()
  skip_on_cran()

  # Check that the response for invalid country throws an error
  expect_error(get_cp(country = "INVALID"), "404")

  # All countries with a povline
  res <- get_cp(country = "all", povline = 2.15)
  expect_true(is.data.frame(res) || inherits(res, "pip_api"))
})


