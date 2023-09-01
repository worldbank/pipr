#Disable caching
Sys.setenv("PIPR_DISABLE_CACHING" = "TRUE")
#To enable running tests which uses real API uncomment the below line
#Sys.setenv("NOT_CRAN" = "true")

## Test core functions ----
test_that("get_aux returns available tables when no argument is specified", {
  skip_if_offline()
  skip_on_cran()

  res <- suppressMessages(get_aux())
  expect_true(tibble::is_tibble(res))
  expect_equal(names(res), "tables")
})


test_that("get_aux() works when calling specific tables", {
  skip_if_offline()
  skip_on_cran()
  # Return tibble as default
  res <- suppressMessages(get_aux())
  expect_true(tibble::is_tibble(res))
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
  # TO DO: Use prod server for this test when API has been released
  # expect_error(get_aux("tmp"))
  # expect_true(is.list(get_aux("tmp", simplify = FALSE)))
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  expect_error(get_aux("wrong-table-name", server = "qa"))
  expect_true(is.list(get_aux("wrong-table-name", simplify = FALSE, server = "prod")))

  # Check all tables
  skip("survey_metadata gives a 500 error. Need to add functionality for list data")
  dl <- lapply(res, function(x) try(get_aux(x)))
  expect_true(all(sapply(dl, tibble::is_tibble)))
  expect_true(sapply(dl, function(x) any(class(x) != "try-error")))
  # expect_false(sapply(dl, function(x) any(names(x) == "error")))
})

test_that("User agent works", {
  skip_if_offline()
  skip_on_cran()
  # res <- get_aux(simplify = FALSE)
  # tmp <- res$response$request$options$useragent
  # expect_identical(tmp, pipr_user_agent)
  res <- get_aux("gdp", simplify = FALSE)
  tmp <- res$response$request$options$useragent
  expect_identical(tmp, pipr_user_agent)
})

## Test helper functions ----
test_that("get_countries() works", {
  skip_if_offline()
  skip_on_cran()
  res <- get_countries()
  res2 <- get_aux("countries")
  expect_true(tibble::is_tibble(res))
  expect_identical(res, res2)
})

test_that("get_regions() works", {
  skip_if_offline()
  skip_on_cran()
  res <- get_regions()
  res2 <- get_aux("regions")
  expect_true(tibble::is_tibble(res))
  expect_identical(res, res2)
})
