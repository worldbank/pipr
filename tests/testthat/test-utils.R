# constants
res_ex_json <- readRDS("../testdata/res-ex-json.RDS")
res_ex_csv <- readRDS("../testdata/res-ex-csv.RDS")
res_ex_rds <- readRDS("../testdata/res-ex-rds.RDS")

# tests
test_that("check_internet() works", {
  expect_true(check_internet())
  expect_identical(check_internet(), curl::has_internet())
  expect_invisible(check_internet())
})

test_that("check_api() works", {
  res <- check_api("v1", server = NULL)
  expect_equal(res$status_code, 200)
  expect_invisible(check_api("v1"))
  expect_error(check_api("xx"))
})

test_that("check_status() works", {
  res <- check_api("v1")
  parsed <- parse_response(res, simplify = FALSE)$content
  expect_true(check_status(res, parsed))
  parsed <- "Invalid query parameters have been submitted"
  expect_error(check_status(res, parsed))
  res$status_code <- 400
  expect_error(check_status(res, parsed))
})

test_that("build_url() works", {

  # Check that url is correctly pasted together
  x <- build_url(server = NULL, endpoint = "pip", api_version = "v1")
  expect_identical(x, paste0(base_url, "/v1/pip"))
  x <- build_url("prod", "pip", api_version = "v1")
  expect_identical(x, paste0(base_url, "/v1/pip"))
  x <- build_url("prod", "pip-grp", api_version = "v2")
  expect_identical(x, paste0(base_url, "/v2/pip-grp"))

  # Expect error if server arg is incorrect
  expect_error(build_url("tmp", "pip", "v1"))

  # Check internal URLs
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  x <- build_url("qa", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_QA_URL"), "/v1/pip"))
  x <- build_url("dev", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_DEV_URL"), "/v1/pip"))
})

test_that("build_args() works for all individual parameters", {

  # country
  x <- build_args(country = "AGO")
  expect_equal(length(x), 1)
  expect_identical(names(x), "country")
  expect_identical(x$country, "AGO")

  # year
  x <- build_args(year = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "year")
  expect_identical(x$year, "all")

  # povline
  x <- build_args(povline = 1.9)
  expect_equal(length(x), 1)
  expect_identical(names(x), "povline")
  expect_identical(x$povline, 1.9)

  # popshare
  x <- build_args(popshare = .5)
  expect_equal(length(x), 1)
  expect_identical(names(x), "popshare")
  expect_identical(x$popshare, .5)

  # fill_gaps
  x <- build_args(fill_gaps = TRUE)
  expect_equal(length(x), 1)
  expect_identical(names(x), "fill_gaps")
  expect_identical(x$fill_gaps, TRUE)

  # group_by
  x <- build_args(group_by = "wb")
  expect_equal(length(x), 1)
  expect_identical(names(x), "group_by")
  expect_identical(x$group_by, "wb")

  # welfare_type
  x <- build_args(welfare_type = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "welfare_type")
  expect_identical(x$welfare_type, "all")

  # reporting_level
  x <- build_args(reporting_level = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "reporting_level")
  expect_identical(x$reporting_level, "all")

  # reporting_level
  x <- build_args(reporting_level = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "reporting_level")
  expect_identical(x$reporting_level, "all")

  # version
  x <- build_args(version = "test")
  expect_equal(length(x), 1)
  expect_identical(names(x), "version")
  expect_identical(x$version, "test")

  # format
  x <- build_args(format = "json")
  expect_equal(length(x), 1)
  expect_identical(names(x), "format")
  expect_identical(x$format, "json")

  # table
  x <- build_args(table = "regions")
  expect_equal(length(x), 1)
  expect_identical(names(x), "table")
  expect_identical(x$table, "regions")
})

test_that("build_args() works for mulitiple parameters", {

  # Multiple parameters
  x <- build_args(country = "AGO", year = 2008, povline = 1.9)
  expect_equal(length(x), 3)
  expect_identical(names(x), c("country", "year", "povline"))
  expect_identical(x$country, "AGO")
  expect_equal(x$year, 2008)
  expect_equal(x$povline, 1.9)

  # Check that NULL arguments are removed
  x <- build_args(country = "AGO", year = 2008, povline = 1.9, group_by = NULL)
  expect_equal(length(x), 3)
  expect_identical(names(x), c("country", "year", "povline"))
  expect_identical(x$country, "AGO")
  expect_equal(x$year, 2008)
  expect_equal(x$povline, 1.9)
})

test_that("build_args() fails when all parameters are NULL", {
  expect_error(build_args(country = NULL))
})

test_that("parse_response() works for different formats", {

  # json
  res <- parse_response(res_ex_json, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_json, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "response")
  expect_identical(class(res$content), "data.frame")

  # csv
  res <- parse_response(res_ex_csv, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_csv, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "response")
  expect_true(all(class(res$content) %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")))

  # rds
  res <- parse_response(res_ex_rds, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_rds, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "response")
  expect_true(all(class(res$content) %in% c("data.table", "data.frame")))
})
