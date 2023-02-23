skip("Skipping online version of the tests")

dev_host <- gsub("/api|http://", "", Sys.getenv("PIP_DEV_URL"))
qa_host <- gsub("/pip|/api|http(s)?://", "", Sys.getenv("PIP_QA_URL"))

test_that("health_check() works", {
  res <- health_check(api_version = "v1")
  expect_identical(httr::content(res)[[1]], "PIP API is running")
  expect_equal(res$status_code, 200)
  expect_invisible(health_check(api_version = "v1"))
  expect_error(health_check("xx"))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  skip_if(is.null(curl::nslookup(dev_host, error = FALSE)), message = "Could not connect to DEV host")
  expect_identical(httr::content(health_check(api_version = "v1", server = "dev"))[[1]], "PIP API is running")
  skip_if(is.null(curl::nslookup(qa_host, error = FALSE)), message = "Could not connect to QA host")
  expect_identical(httr::content(health_check(api_version = "v1", server = "qa"))[[1]], "PIP API is running")
})

test_that("get_versions() works", {

  # res <- get_versions() # TO DO: Use prod server for this test when API has been released
  # expect_true(tibble::is_tibble(res))

  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  res <- get_versions(server = "qa")
  expect_true(tibble::is_tibble(res))
  res <- get_versions(server = "dev")
  expect_true(tibble::is_tibble(res))
  skip_if(is.null(curl::nslookup(qa_host, error = FALSE)), message = "Could not connect to QA host")
  res <- get_versions(server = "qa")
  expect_true(tibble::is_tibble(res))
})

test_that("get_pip_info() works", {
  res <- get_pip_info()
  expect_true(is.list(res))
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  skip_if(is.null(curl::nslookup(dev_host, error = FALSE)), message = "Could not connect to DEV host")
  res <- get_pip_info(server = "dev")
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
  skip_if(is.null(curl::nslookup(qa_host, error = FALSE)), message = "Could not connect to QA host")
  res <- get_pip_info(server = "qa")
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
})
