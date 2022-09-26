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

  # Return tibble as default
  res <- get_aux()
  expect_true(tibble::is_tibble(res))
  res <- get_aux("gdp")
  expect_true(tibble::is_tibble(res))
  res <- get_aux("countries", version = NULL, api_version = "v1", format = "rds", server = NULL)
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
  expect_error(get_aux("tmp", server = "qa"))
  expect_true(is.list(get_aux("tmp", simplify = FALSE, server = "qa")))

  # Check all tables
  skip("survey_metadata gives a 500 error. Need to add functionality for list data")
  dl <- lapply(res, function(x) try(get_aux(x)))
  expect_true(all(sapply(dl, tibble::is_tibble)))
  expect_true(sapply(dl, function(x) any(class(x) != "try-error")))
  # expect_false(sapply(dl, function(x) any(names(x) == "error")))
})

test_that("User agent works", {
  # res <- get_aux(simplify = FALSE)
  # tmp <- res$response$request$options$useragent
  # expect_identical(tmp, pipr_user_agent)
  res <- get_aux("gdp", simplify = FALSE)
  tmp <- res$response$request$options$useragent
  expect_identical(tmp, pipr_user_agent)
})

