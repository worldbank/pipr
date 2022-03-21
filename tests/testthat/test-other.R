test_that("health_check() works", {
  expect_identical(health_check(), "PIP API is running")
  expect_error(health_check("xx"))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  expect_identical(health_check(server = "dev"), "PIP API is running")
  expect_identical(health_check(server = "qa"), "PIP API is running")
})

test_that("get_versions() works", {
  res <- get_versions()
  expect_true(is.character(res))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  res <- get_versions(server = "dev")
  expect_true(is.character(res))
  res <- get_versions(server = "qa")
  expect_true(is.character(res))
})

test_that("get_pip_info() works", {
  res <- get_pip_info()
  expect_true(is.list(res))
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  res <- get_pip_info(server = "dev")
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
  res <- get_pip_info(server = "qa")
  expect_identical(names(res), c(
    "available_data_versions", "package_versions",
    "r_version", "server_os", "server_time"
  ))
})
