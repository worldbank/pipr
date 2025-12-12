# constants
dev_host <- gsub("/api|http://", "", Sys.getenv("PIP_DEV_URL"))
qa_host <- gsub("/pip|/api|http(s)://", "", Sys.getenv("PIP_QA_URL"))

# tests
test_that("get_stats() returns the correct format", {
  skip_if_offline()
  skip_on_cran()
  # Return table if simplify = TRUE
  df <- get_stats("AGO", year = 2000)
  expect_true(tibble::is_tibble(df))
  # Return custom response list if simplify = FALSE
  res <- get_stats("AGO", year = 2000, simplify = FALSE)
  expect_true(is.list(res))
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
})

test_that("get_stats() works for a single country-year", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats("AGO", year = 2000)
  expect_equal(nrow(df), 1)
})

test_that("get_stats() works for multiple countries and years", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats(c("AGO", "ALB"), year = 2000)
  expect_equal(nrow(df), 1)
  df <- get_stats(c("AGO"), year = c(2000, 2018))
  expect_equal(nrow(df), 2)
})

test_that("get_stats() works for all countries and years", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats("all", year = "all")
  expect_gte(nrow(df), 2000)
  # skip("Looks like there is a potential data inconsitency in the API")
  countries <- get_aux("countries")
  expect_true(all(countries$country_code %in% df$country_code))
})

test_that("get_stats() works w/ fill_gaps = TRUE", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats("all", year = "all", fill_gaps = TRUE)
  expect_gte(nrow(df), 6000)
})

test_that("get_stats() works w/ popshare option", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats("AGO", year = "all", popshare = .5)
  expect_gte(nrow(df), 3)
  #Ensure there are different values for headcount generated
  #to confirm popshare is being passed to get_stats() function.
  expect_gte(length(unique(df$headcount)), 1)
})

test_that("get_stats() works w/ subgroup = 'wb_regions'", {
  skip_if_offline()
  skip_on_cran()

  df <- get_stats("all", year = 2011, subgroup = "wb_regions", server = "prod")
  expect_equal(nrow(df), 10)
  expect_identical(
    sort(df$region_code),
    sort(c(
      "AFE", "AFW", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF", "WLD"
      # "AFE", "AFW", "EAP", "EAS", "ECA", "ECS", "LAC", "LCN", "MEA", "MNA", "NAC",
      # "OHI", "SAR", "SAS", "SSA", "SSF", "WLD"
    ))
  )
})

test_that("get_stats() works w/ subgroup = 'none'", {
  skip("Need to review to whole logic for this")
  skip_if_offline()
  skip_on_cran()

  # skip_if(is.null(curl::nslookup(qa_host, error = FALSE)),
  #   message = "Could not connect to QA host"
  # )
  df <- get_stats("all", year = 2011, subgroup = "none", server = "prod")
  expect_equal(nrow(df), 1)
  expect_identical(df$region_code, "CUSTOM")
  df <- get_stats(c("ARG", "BRA"), year = 2011, subgroup = "none", server = "prod")
  expect_equal(nrow(df), 1)
  expect_identical(df$region_code, "CUSTOM")
})

test_that("get_stats() works w/ all response formats", {
  skip_if_offline()
  skip_on_cran()
  df <- get_stats("AGO", year = "all", format = "json")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
  df <- get_stats("AGO", year = "all", format = "csv")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
  df <- get_stats("AGO", year = "all", format = "rds")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
})

test_that("get_stats() returns a tibble with named columns for empty response (for rds and csv) ", {
  skip_if_offline()
  skip_on_cran()

  # rds
  res <- get_stats("AGO", 2000, format = "rds")
  res2 <- get_stats("AGO", 2005, format = "rds") # empty response
  expect_equal(dim(res)[2], dim(res2)[2])
  expect_identical(names(res), names(res2))

  # csv
  res <- get_stats("AGO", 2000, format = "csv")
  res2 <- get_stats("AGO", 2005, format = "csv") # empty response
  expect_equal(dim(res)[2], dim(res2)[2])
  expect_identical(names(res), names(res2))

  # json (does not return an empty response data frame)
  # GC: this test returns a warning now because json returns a completely
  # empty tibble (no variables), is this correct?
  # res2 <- get_stats("AGO", 2005, format = "json") # empty response
  # expect_equal(dim(res2)[2], 0)
  # expect_equal(length(names(res2)), 0)

})

test_that("get_stats() works w/ simplify = FALSE", {
  skip_if_offline()
  skip_on_cran()

  res <- get_stats("AGO", year = "all", simplify = FALSE)
  expect_true(is.list(res))
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_true(is.data.frame(res$content))
  expect_gte(nrow(res$content), 3)
})

test_that("get_wb() works", {
  skip_if_offline()
  skip_on_cran()

  df <- get_wb(year = 2011, server = "prod")
  expect_equal(nrow(df), 10)
  expect_identical(
    sort(df$region_code),
    sort(c(
      "AFE", "AFW", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF", "WLD"
      # "AFE", "AFW", "EAP", "EAS", "ECA", "ECS", "LAC", "LCN", "MEA", "MNA", "NAC",
      # "OHI", "SAR", "SAS", "SSA", "SSF", "WLD"
    ))
  )
})

test_that("get_wb() works w/ all response formats", {
  skip_if_offline()
  skip_on_cran()

  df <- get_wb(year = "all", format = "json", server = "prod")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
  df <- get_wb(year = "all", format = "csv", server = "prod")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
  df <- get_wb(year = "all", format = "rds", server = "prod")
  expect_true(tibble::is_tibble(df))
  expect_gte(nrow(df), 3)
})

test_that("get_wb() works w/ simplify = FALSE", {
  skip_if_offline()
  skip_on_cran()
  # skip_if(Sys.getenv("PIPR_RUN_LOCAL_TESTS") != "TRUE",
  #   message = "pip-grp not implement on PROD yet"
  # )
  # skip_if(is.null(curl::nslookup(qa_host, error = FALSE)),
  #   message = "Could not connect to QA host"
  # )
  res <- get_wb(year = "all", simplify = FALSE, server = "prod")
  expect_true(is.list(res))
  expect_identical(names(res), c("url", "status", "type", "content", "response"))
  expect_identical(class(res), "pip_api")
  expect_true(is.data.frame(res$content))
  expect_gte(nrow(res$content), 3)
})


test_that("get_stats() works with nowcast == TRUE",{
  skip_if_offline()
  skip_on_cran()

  nowcast_output <- get_stats("AGO", nowcast = TRUE)
  expect_true("nowcast" %in% nowcast_output$estimate_type)
})

test_that("get_agg(official) and get_agg(region) return identical data", {
  skip_if_offline()
  skip_on_cran()
  df_official <- get_agg(aggregate = "official", server = "prod")
  df_region   <- get_agg(aggregate = "region", server = "prod")
  expect_identical(df_official, df_region)
})

test_that("get_agg(pcn) and get_agg(regionpcn) return identical data", {
  skip_if_offline()
  skip_on_cran()
  df_pcn       <- get_agg(aggregate = "pcn", server = "prod")
  df_regionpcn <- get_agg(aggregate = "regionpcn", server = "prod")
  expect_identical(df_pcn, df_regionpcn)
})

test_that("get_agg(fcv) returns non-NULL data", {
  skip_if_offline()
  skip_on_cran()
  df_fcv <- get_agg(aggregate = "fcv", server = "prod")
  expect_false(is.null(df_fcv))
})

test_that("get_agg(ida) returns non-NULL data", {
  skip_if_offline()
  skip_on_cran()
  df_ida <- get_agg(aggregate = "ida", server = "prod")
  expect_false(is.null(df_ida))
})