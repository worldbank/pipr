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

test_that("get_countries() works", {
  res <- get_countries()
  res2 <- get_aux("countries")
  expect_true(tibble::is_tibble(res))
  expect_identical(res, res2)
})

test_that("get_regions() works", {
  res <- get_regions()
  res2 <- get_aux("regions")
  expect_true(tibble::is_tibble(res))
  expect_identical(res, res2)
})

test_that("get_aux() works", {

  # Return character vector with tables if table = NULL
  res <- get_aux()
  expect_true(is.character(res))

  # Return tibble as default
  res <- get_aux("gdp")
  expect_true(tibble::is_tibble(res))

  # Return custom response list if simplify FALSE
  res <- get_aux("gdp", simplify = FALSE)
  expect_true(is.list(res))
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")

  # Check that all formats works
  res <- get_aux("gdp", format = "json")
  expect_true(tibble::is_tibble(res))
  res <- get_aux("gdp", format = "csv")
  expect_true(tibble::is_tibble(res))
  res <- get_aux("gdp", format = "rds")
  expect_true(tibble::is_tibble(res))

  # Check failure if table doesn't exist
  expect_error(get_aux("tmp"))
  expect_error(get_aux("tmp", simplify = FALSE))

  # Check all tables
  skip("survey_metadata gives a 500 error. Need to add functionality for list data")
  dl <- lapply(res, function(x) try(get_aux(x)))
  expect_true(all(sapply(dl, tibble::is_tibble)))
  expect_true(sapply(dl, function(x) any(class(x) != "try-error")))
  # expect_false(sapply(dl, function(x) any(names(x) == "error")))
})