#Disable caching
Sys.setenv("PIPR_DISABLE_CACHING" = "TRUE")
#To enable running tests which uses real API uncomment the below line
#Sys.setenv('NOT_CRAN' = 'true')

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

test_that("get_aux() works", {
  skip_if_offline()
  skip_on_cran()
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
  skip_if_offline()
  skip_on_cran()
  # res <- get_aux(simplify = FALSE)
  # tmp <- res$response$request$options$useragent
  # expect_identical(tmp, pipr_user_agent)
  res <- get_aux("gdp", simplify = FALSE)
  tmp <- res$response$request$options$useragent
  expect_identical(tmp, pipr_user_agent)
})


test_that("get_countries() with mocking works", {
  mockery::stub(get_aux, "httr::GET", function(...) {
    readRDS('../testdata/response-country.RDS')
  })

  mockery::stub(get_countries, "get_aux", function(...) {
    readRDS('../testdata/response-country.RDS')
  })

  res1 <- get_aux('countries')
  res2 <- parse_response(get_countries(), TRUE)

  expect_true(tibble::is_tibble(res1))
  expect_true(tibble::is_tibble(res2))

  expect_equal(dim(res1), dim(res2))
  expect_identical(res1, res2)
})


test_that("get_regions() with mocking works", {
  mockery::stub(get_aux, "httr::GET", function(...) {
    readRDS('../testdata/response-regions.RDS')
  })

  mockery::stub(get_regions, "get_aux", function(...) {
    readRDS('../testdata/response-regions.RDS')
  })

  res1 <- get_aux('regions')
  res2 <- parse_response(get_regions(), TRUE)

  expect_true(tibble::is_tibble(res1))
  expect_true(tibble::is_tibble(res2))

  expect_equal(dim(res1), dim(res2))
  expect_identical(res1, res2)
})


test_that("get_cpi() with mocking works", {
  mockery::stub(get_aux, "httr::GET", function(...) {
    readRDS('../testdata/response-cpi.RDS')
  })

  mockery::stub(get_cpi, "get_aux", function(...) {
    readRDS('../testdata/response-cpi.RDS')
  })

  res1 <- get_aux('cpi')
  res2 <- parse_response(get_cpi(), TRUE)

  expect_true(tibble::is_tibble(res1))
  expect_true(tibble::is_tibble(res2))

  expect_equal(dim(res1), dim(res2))
  expect_identical(res1, res2)
})

test_that("get_dictionary() with mocking works", {
  mockery::stub(get_aux, "httr::GET", function(...) {
    readRDS('../testdata/response-dictionary.RDS')
  })

  mockery::stub(get_dictionary, "get_aux", function(...) {
    readRDS('../testdata/response-dictionary.RDS')
  })

  res1 <- get_aux('dictionary')
  res2 <- parse_response(get_dictionary(), TRUE)

  expect_true(tibble::is_tibble(res1))
  expect_true(tibble::is_tibble(res2))

  expect_equal(dim(res1), dim(res2))
  expect_identical(res1, res2)
})

