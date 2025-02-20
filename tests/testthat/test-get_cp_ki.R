
# 1. General Argument Matching Tests ----
test_that("Argument matching works correctly for get_cp_ki()", {
  skip_if_offline()
  skip_on_cran()

  # Default arguments, one country, simplify = TRUE
  res <- get_cp_ki(country = "IDN")
  expect_type(res, "list")

  # Default arguments, one country, simplify = FALSE
  res <- get_cp_ki(country = "IDN", simplify = FALSE)
  expect_s3_class(res, "pip_api")

  # Argument matching for 'format'
  # expect_error(get_cp_ki(format = "txt"), "'arg' should be one of")

  # Argument matching for 'api_version'
  expect_error(get_cp_ki(api_version = "v2"), "'arg' should be")
})

# 2. povline Set-up Tests ----
test_that("povline and ppp_version arguments work correctly for get_cp_ki()", {
  skip_if_offline()
  skip_on_cran()

  # Default povline
  res <- get_cp_ki(country = "IDN")
  expect_true(any(res$poverty_line == 2.15))

  res <- get_cp_ki(country = "IDN", ppp_version = 2017)
  expect_true(any(res$poverty_line == 2.15))

  # povline with ppp_version 2011
  res <- get_cp_ki(country = "IDN", ppp_version = 2011, povline = NULL)
  expect_true(any(res$poverty_line == 1.9))

  # povline when povline is provided
  # res <- get_cp_ki(country = "IDN", povline = 3.2)
  # expect_true(any(res$poverty_line == 3.2))
})

# 3. Country Argument Tests ----
test_that("Country argument validation works correctly in get_cp_ki()", {
  skip_if_offline()
  skip_on_cran()

  # Valid country
  res <- get_cp_ki(country = "IDN")
  expect_type(res, "list")

  # Missing country argument
  expect_error(get_cp_ki(country = NULL), "Please provide a country code.")

  # More than one country
  expect_error(get_cp_ki(country = c("IDN", "AGO")), "Please provide only one country code.")
})



# 4. Request Execution Tests ----
test_that("Requests threws error for get_cp_ki()", {
  skip_if_offline()
  skip_on_cran()

  # Check that the response for invalid country throws an error
  expect_error(get_cp_ki(country = "INVALID"), "404")
})

# 6. Response Parsing and Unnesting Tests ----
test_that("Response parsing and unnesting work correctly for get_cp_ki()", {
  skip_if_offline()
  skip_on_cran()

  # Build unnested dataset manually
  res_manual <- data.frame(
    country_code = "IDN",
    reporting_year = 2023,
    poverty_line = 2.15,
    headcount = 0.0182,
    headcount_national = 9.4,
    mpm_headcount = 0.0214,
    reporting_pop = 277.5341,
    gni = 4870,
    latest_gni = TRUE,
    gdp_growth = 5.0481,
    latest_gdp = TRUE,
    year_range = "2018-2023",
    share_below_40 = 2.7768,
    share_total = 1.8967,
    stringsAsFactors = FALSE
  )

  res <- get_cp_ki(country = "IDN", simplify = TRUE)
  expect_equal(res, res_manual)
})

