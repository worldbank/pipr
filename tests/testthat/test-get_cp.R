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
  expect_error(get_cp(format = "txt"), "Invalid `format`")

  # Argument matching for 'api_version'
  expect_error(get_cp(api_version = "v2"), "Invalid `api_version`")

})

# 1.5. Shared Argument Validation Tests ----
test_that("get_cp() rejects malformed shared arguments before HTTP", {
  expect_error(get_cp(country = "AG"), "country")
  expect_error(get_cp(country = "ago"), "country")
  expect_error(get_cp(povline = -1), "povline")
  expect_error(get_cp(povline = "2.15"), "povline")
  expect_error(get_cp(version = "20260324"), "version")
  expect_error(get_cp(version = "20260324_2021_01_02"), "version")
  expect_error(get_cp(ppp_version = "2017a"), "ppp_version")
  expect_error(get_cp(release_version = "2024-06-27"), "release_version")
  expect_error(get_cp(simplify = "TRUE"), "simplify")
  expect_error(get_cp(server = 123), "server")
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

  # povline with ppp_version 2011 is covered by the mocked tests below
  # (the live API errors for ppp_version = 2011)

  # povline when povline is provided
  res <- get_cp(country = "AGO", povline = 3.2)
  expect_true(any(res$poverty_line == 3.2))
})

test_that("get_cp() sends povline = 1.9 when ppp_version = 2011 and povline is NULL", {
  # Mocked: the live API errors for ppp_version = 2011, so assert on the
  # request URL instead of the response
  captured_url <- NULL
  mock_res <- structure(
    list(
      url = "http://mock-api/cp-download",
      status_code = 200,
      body = charToRaw("{}"),
      headers = list("content-type" = "application/json")
    ),
    class = "httr2_response"
  )

  local_mocked_bindings(
    req_perform = function(req) {
      captured_url <<- req$url
      mock_res
    },
    .package = "httr2"
  )

  suppressWarnings(get_cp(country = "AGO", ppp_version = 2011, povline = NULL))

  expect_true(grepl("povline=1.9", captured_url))
})

test_that("get_cp() sends the default povline when ppp_version is not 2011", {
  captured_url <- NULL
  mock_res <- structure(
    list(
      url = "http://mock-api/cp-download",
      status_code = 200,
      body = charToRaw("{}"),
      headers = list("content-type" = "application/json")
    ),
    class = "httr2_response"
  )

  local_mocked_bindings(
    req_perform = function(req) {
      captured_url <<- req$url
      mock_res
    },
    .package = "httr2"
  )

  suppressWarnings(get_cp(country = "AGO", ppp_version = 2017, povline = NULL))

  # The 2.15 default is applied server-side; the client sends no povline
  expect_false(grepl("povline=1.9", captured_url))
  expect_false(grepl("povline=", captured_url))
})


# 3. Other Tests ----
test_that("Requests execute successfully for get_cp()", {
  skip_if_offline()
  skip_on_cran()

  # Check that the response for invalid country throws an error
  expect_error(get_cp(country = "INVALID"), "country")

  # All countries with a povline
  res <- get_cp(country = "all", povline = 2.15)
  expect_true(is.data.frame(res) || inherits(res, "pip_api"))
})


