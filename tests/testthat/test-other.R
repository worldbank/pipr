
test_that("get_versions() works and returns a tibble", {

  # GC Note: Not sure about this but I'll leave it:
  skip_if_offline()
  skip_on_cran()

  # Mock json
  mock_json_response <- jsonlite::toJSON(
    list(
      list(version = "20240627_2017_01_02_PROD",
           release_version = "20240627",
           ppp_version = "2017",
           identity = "PROD"),
      list(version = "20240326_2011_02_02_PROD",
           release_version = "20240326",
           ppp_version = "2011",
           identity = "PROD")
    ), auto_unbox = TRUE
  )

  # mock res
  mock_res <- structure(
    list(
      url = "http://mock-api/versions",
      status_code = 200,
      body = charToRaw(mock_json_response),
      headers = list("content-type" = "application/json")
    ),
    class = "httr2_response"
  )

  # local mocked for httr2
  local_mocked_bindings(
    req_perform = function(req) mock_res,
    .package = "httr2"
  )

  #mocked function
  res <- get_versions()

  # tests
  expect_true(tibble::is_tibble(res))  # Ensure the result is a tibble
  expect_equal(ncol(res), 4)           # Ensure correct number of columns
  expect_equal(nrow(res), 2)           # Ensure correct number of rows
  expect_equal(res$version[1], "20240627_2017_01_02_PROD")
  expect_equal(res$release_version[2], "20240326")
})


