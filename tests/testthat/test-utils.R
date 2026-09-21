# constants
res_ex_json <- readRDS(test_path("testdata", "res-ex-json.RDS"))
res_ex_csv <- readRDS(test_path("testdata", "res-ex-csv.RDS"))
res_ex_rds <- readRDS(test_path("testdata", "res-ex-rds.RDS"))
res_ex_404 <- readRDS(test_path("testdata", "res-ex-404.RDS"))
dictionary <- readRDS(test_path("testdata", "dictionary.RDS"))

# Shared argument validators ----
test_that("validate_country() accepts valid values", {
  expect_identical(validate_country("AGO"), "AGO")
  expect_identical(validate_country("all"), "all")
  expect_identical(validate_country(c("AGO", "ALB")), c("AGO", "ALB"))
  # "all" may be mixed with codes; each element must be "all" or an ISO 3 code
  expect_identical(validate_country(c("all", "AGO")), c("all", "AGO"))
})

test_that("validate_country() rejects invalid values", {
  expect_error(validate_country("AG"), "country")
  expect_error(validate_country("ago"), "country")
  expect_error(validate_country("AGO1"), "country")
  expect_error(validate_country(character()), "country")
  expect_error(validate_country(c("AGO", NA)), "country")
})

test_that("validate_year() accepts valid values", {
  expect_identical(validate_year(2000), 2000)
  expect_identical(validate_year(c(2000, 2018)), c(2000, 2018))
  expect_identical(validate_year("all"), "all")
  expect_identical(validate_year("MRV"), "MRV")
})

test_that("validate_year() rejects invalid values", {
  expect_error(validate_year(2000.5), "year")
  expect_error(validate_year("2000"), "year")
  expect_error(validate_year("MRVX"), "year")
  expect_error(validate_year(integer()), "year")
  expect_error(validate_year(c(2000, NA)), "year")
})

test_that("validate_povline() accepts valid values", {
  expect_identical(validate_povline(2.15), 2.15)
  expect_identical(validate_povline(0), 0)
  expect_null(validate_povline(NULL))
})

test_that("validate_povline() rejects invalid values", {
  expect_error(validate_povline(-1), "povline")
  expect_error(validate_povline("2.15"), "povline")
  expect_error(validate_povline(c(2.15, 3.2)), "povline")
  expect_error(validate_povline(Inf), "povline")
})

test_that("validate_popshare() accepts valid values", {
  expect_identical(validate_popshare(0.4), 0.4)
  expect_identical(validate_popshare(0), 0)
  expect_identical(validate_popshare(1), 1)
  expect_null(validate_popshare(NULL))
})

test_that("validate_popshare() rejects invalid values", {
  expect_error(validate_popshare(-0.1), "popshare")
  expect_error(validate_popshare(1.5), "popshare")
  expect_error(validate_popshare("0.4"), "popshare")
  expect_error(validate_popshare(c(0.4, 0.5)), "popshare")
})

test_that("validate_logical() accepts valid values", {
  expect_true(validate_logical(TRUE, "fill_gaps"))
  expect_false(validate_logical(FALSE, "fill_gaps"))
})

test_that("validate_logical() rejects invalid values", {
  expect_error(validate_logical("TRUE", "fill_gaps"), "fill_gaps")
  expect_error(validate_logical(NA, "fill_gaps"), "fill_gaps")
  expect_error(validate_logical(1, "fill_gaps"), "fill_gaps")
})

test_that("validate_subgroup() accepts valid values", {
  expect_identical(validate_subgroup("none"), "none")
  expect_identical(validate_subgroup("wb_regions"), "wb_regions")
  expect_null(validate_subgroup(NULL))
})

test_that("validate_subgroup() rejects invalid values", {
  expect_error(validate_subgroup("wb"), "subgroup")
  expect_error(validate_subgroup("regions"), "subgroup")
  expect_error(validate_subgroup(c("none", "wb_regions")), "subgroup")
})

test_that("validate_version() accepts valid values", {
  expect_identical(
    validate_version("20260324_2021_01_02_PROD"),
    "20260324_2021_01_02_PROD"
  )
  # Structural format only: components are not semantically validated
  # (identity suffix is not limited to PROD; see get_versions())
  expect_identical(
    validate_version("20260324_9999_99_99_QA"),
    "20260324_9999_99_99_QA"
  )
  expect_null(validate_version(NULL))
})

test_that("validate_version() rejects invalid values", {
  expect_error(validate_version(2024), "version")
  expect_error(validate_version(""), "version")
  expect_error(validate_version(NA), "version")
  expect_error(validate_version("20260324"), "version")
  expect_error(validate_version("20260324_2021_01_02"), "version")
  expect_error(validate_version("20260324_2021_01_02_PROD_extra"), "version")
})

test_that("validate_ppp_version() accepts valid values", {
  expect_identical(validate_ppp_version(2017), 2017)
  expect_identical(validate_ppp_version("2021"), "2021")
  expect_null(validate_ppp_version(NULL))
})

test_that("validate_ppp_version() rejects invalid values", {
  expect_error(validate_ppp_version("2017a"), "ppp_version")
  expect_error(validate_ppp_version(c(2017, 2011)), "ppp_version")
  expect_error(validate_ppp_version(NA), "ppp_version")
  expect_error(validate_ppp_version(2017.5), "ppp_version")
  expect_error(validate_ppp_version(Inf), "ppp_version")
})

test_that("validate_release_version() accepts valid values", {
  expect_identical(validate_release_version("20240627"), "20240627")
  expect_null(validate_release_version(NULL))
})

test_that("validate_release_version() rejects invalid values", {
  expect_error(validate_release_version("2024-06-27"), "release_version")
  expect_error(validate_release_version("2024062"), "release_version")
  expect_error(validate_release_version("20241301"), "release_version")
  expect_error(validate_release_version(20240627), "release_version")
})

test_that("validate_simplify() accepts valid values", {
  expect_true(validate_simplify(TRUE))
  expect_false(validate_simplify(FALSE))
})

test_that("validate_simplify() rejects invalid values", {
  expect_error(validate_simplify("TRUE"), "simplify")
  expect_error(validate_simplify(NA), "simplify")
  expect_error(validate_simplify(c(TRUE, FALSE)), "simplify")
  expect_error(validate_simplify(1), "simplify")
})

test_that("validate_server() accepts valid values", {
  expect_identical(validate_server("prod"), "prod")
  expect_null(validate_server(NULL))
})

test_that("validate_server() rejects invalid values", {
  expect_error(validate_server(123), "server")
  expect_error(validate_server(""), "server")
  expect_error(validate_server(NA), "server")
})

# tests
test_that("check_internet() works", {
  skip_if_offline()
  skip_on_cran()
  expect_true(check_internet())
  expect_identical(check_internet(), curl::has_internet())
  expect_invisible(check_internet())
})

test_that("check_api() works", {
  skip_if_offline()
  skip_on_cran()
  res <- check_api("v1")
  expect_equal(res, "PIP API is running")
})

test_that("check_status() works", {
  skip_if_offline()
  skip_on_cran()
  # 200
  res <- health_check("v1")
  expect_true(check_status(res))

  # 404
  res <- res_ex_404
  expect_error(check_status(res))

  # 500
  res <- res_ex_404
  res$status_code <- 500
  expect_error(check_status(res))

})

test_that("build_base_url() works", {

  # Check that url is correctly pasted together
  x <- build_base_url(server = NULL, endpoint = "pip", api_version = "v1")
  expect_identical(x, paste0(prod_url, "/v1/pip"))
  x <- build_base_url("prod", "pip", api_version = "v1")
  expect_identical(x, paste0(prod_url, "/v1/pip"))
  x <- build_base_url("prod", "pip-grp", api_version = "v2")
  expect_identical(x, paste0(prod_url, "/v2/pip-grp"))

  # Expect error if server arg is incorrect
  expect_error(build_base_url("tmp", "pip", "v1"))

  # Check internal URLs
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  x <- build_base_url("qa", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_QA_URL"), "/v1/pip"))
  x <- build_base_url("dev", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_DEV_URL"), "/v1/pip"))

  # Expect error if ENV vars are not found
  skip_if(Sys.getenv("PIP_QA_URL") != "")
  expect_error(build_base_url("qa", "pip", "v1"))
  skip_if(Sys.getenv("PIP_DEV_URL") != "")
  expect_error(build_base_url("dev", "pip", "v1"))
})

test_that("build_base_url() works for internal URLS", {

  # Check internal URLs
  skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE")
  x <- build_base_url("qa", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_QA_URL"), "/v1/pip"))
  x <- build_base_url("dev", "pip", "v1")
  expect_identical(x, paste0(Sys.getenv("PIP_DEV_URL"), "/v1/pip"))
})

test_that("build_base_url() throws error for internal URLs if ENV vars are not found", {

  # Expect error if ENV vars are not found
  skip_if(Sys.getenv("PIP_QA_URL") != "")
  expect_error(build_base_url("qa", "pip", "v1"))
  skip_if(Sys.getenv("PIP_DEV_URL") != "")
  expect_error(build_base_url("dev", "pip", "v1"))
})

test_that("build_args() works for all individual parameters", {

  # country
  x <- build_args(.country = "AGO")
  expect_equal(length(x), 1)
  expect_identical(names(x), "country")
  expect_identical(x$country, "AGO")
  x <- build_args(.country = c("ARG", "BRA"))
  expect_equal(length(x), 1)
  expect_identical(names(x), "country")
  expect_identical(x$country, "ARG,BRA")

  # year
  x <- build_args(.year = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "year")
  expect_identical(x$year, "all")
  x <- build_args(.year = c(2008, 2009))
  expect_equal(length(x), 1)
  expect_identical(names(x), "year")
  expect_identical(x$year, "2008,2009")

  # povline
  x <- build_args(.povline = 1.9)
  expect_equal(length(x), 1)
  expect_identical(names(x), "povline")
  expect_identical(x$povline, 1.9)

  # popshare
  x <- build_args(.popshare = .5)
  expect_equal(length(x), 1)
  expect_identical(names(x), "popshare")
  expect_identical(x$popshare, .5)

  # fill_gaps
  x <- build_args(.fill_gaps = TRUE)
  expect_equal(length(x), 1)
  expect_identical(names(x), "fill_gaps")
  expect_identical(x$fill_gaps, TRUE)

  # group_by
  x <- build_args(.group_by = "wb")
  expect_equal(length(x), 1)
  expect_identical(names(x), "group_by")
  expect_identical(x$group_by, "wb")

  # welfare_type
  x <- build_args(.welfare_type = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "welfare_type")
  expect_identical(x$welfare_type, "all")

  # reporting_level
  x <- build_args(.reporting_level = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "reporting_level")
  expect_identical(x$reporting_level, "all")

  # reporting_level
  x <- build_args(.reporting_level = "all")
  expect_equal(length(x), 1)
  expect_identical(names(x), "reporting_level")
  expect_identical(x$reporting_level, "all")

  # version
  x <- build_args(.version = "test")
  expect_equal(length(x), 1)
  expect_identical(names(x), "version")
  expect_identical(x$version, "test")

  # format
  x <- build_args(.format = "json")
  expect_equal(length(x), 1)
  expect_identical(names(x), "format")
  expect_identical(x$format, "json")

  # table
  x <- build_args(.table = "regions")
  expect_equal(length(x), 1)
  expect_identical(names(x), "table")
  expect_identical(x$table, "regions")
})

test_that("build_args() works for mulitiple parameters", {

  # Multiple parameters
  x <- build_args(.country = "AGO", .year = 2008, .povline = 1.9)
  expect_equal(length(x), 3)
  expect_identical(names(x), c("country", "year", "povline"))
  expect_identical(x$country, "AGO")
  expect_equal(x$year, 2008)
  expect_equal(x$povline, 1.9)

  # Check that NULL arguments are removed
  x <- build_args(.country = "AGO", .year = 2008, .povline = 1.9, .group_by = NULL)
  expect_equal(length(x), 3)
  expect_identical(names(x), c("country", "year", "povline"))
  expect_identical(x$country, "AGO")
  expect_equal(x$year, 2008)
  expect_equal(x$povline, 1.9)
})

test_that("build_args() fails when all parameters are NULL", {
  expect_error(build_args(.country = NULL))
})

test_that("parse_response() works for different formats", {

  # json
  res <- parse_response(res_ex_json, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_json, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "httr2_response")
  expect_identical(class(res$content), "data.frame")

  # csv
  res <- parse_response(res_ex_csv, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_csv, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "httr2_response")
  expect_true(all(class(res$content) %in% c("spec_tbl_df", "tbl_df", "tbl", "data.frame")))

  # rds
  res <- parse_response(res_ex_rds, simplify = TRUE)
  expect_true(all(class(res) %in% c("tbl_df", "tbl", "data.frame")))
  res <- parse_response(res_ex_rds, simplify = FALSE)
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_identical(class(res$response), "httr2_response")
  expect_true(all(class(res$content) %in% c("data.table", "data.frame")))
})


test_that("Temporay renaming of response columns work", {

  # Rename when simplify = TRUE
  res <- parse_response(res_ex_json, simplify = TRUE)
  expect_true(all(c("welfare_time", "year", "pop", "gdp", "hfce") %in% names(res)))
  expect_false(all(c("survey_year", "reporting_year",
                     "reporting_pop", "reporting_gdp",
                     "reporting_pce") %in% names(res)))

  # Don't rename when simplify = FALSE
  res <- parse_response(res_ex_json, simplify = FALSE)$content
  expect_false(all(c("welfare_time", "year", "pop", "gdp", "hfce") %in% names(res)))
  expect_true(all(c("survey_year", "reporting_year",
                     "reporting_pop", "reporting_gdp",
                     "reporting_pce") %in% names(res)))
})

skip("No longer necessary. To be fully deprecated soon.")
test_that("Temporary renaming of response works for row-based datasets (dictionary)", {

  skip_on_cran()
  skip_if_offline()
  res <- tmp_rename_cols(dictionary,
                  url = "https://api.worldbank.org/pip/v1/aux?table=dictionary&format=rds")

  expect_true(all(c("welfare_time", "year", "pop", "gdp",
                    "hfce", "hfce_data_level") %in%
                    res$variable))
  expect_false(all(c("survey_year", "reporting_year",
                     "reporting_pop", "reporting_gdp",
                     "reporting_pce", "pce_data_level")
                   %in% res$variable))

})

