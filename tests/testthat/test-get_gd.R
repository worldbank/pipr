# Section 1: General Argument Checks ----
test_that("General Argument Checks", {
  skip_if_offline()
  skip_on_cran()

  # Check if cum_welfare and cum_population have different lengths
  cum_welfare <- c(0.1, 0.2, 0.3)
  cum_population <- c(0.1, 0.2)

  expect_error(
    get_gd(cum_welfare = cum_welfare, cum_population = cum_population, estimate = "stats"),
    "must have the same length"
  )
})

## Section 2: Testing 'stats' Endpoint -----
test_that("'stats' Endpoint Tests", {
  skip_if_offline()
  skip_on_cran()
  # Error when requested_mean or povline is missing
  cum_welfare <- datt_rural$L
  cum_population <- datt_rural$p

  # Missing requested_mean
  expect_error(
    get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
           estimate = "stats", povline = 89),
    "must be provided"
  )

  # Missing povline
  expect_error(
    get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
           estimate = "stats", requested_mean = 109.9),
    "must be provided"
  )

  # Correct retrieval of statistics
  stats <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
                  estimate = "stats", requested_mean = 109.9, povline = 89)

  expect_s3_class(stats, "data.frame") # The returned object should be a data frame
  #expect_true(all(c("poverty_line", "mean", "headcount") %in% names(stats)))

  # popshare
  stats <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
                  estimate = "stats", requested_mean = 109.9, popshare = 0.5,
                  povline = 89)

  expect_true(stats$headcount == 0.5)
})

### Section 3: Testing 'lorenz' Endpoint ----
test_that("'lorenz' Endpoint Tests", {
  skip_if_offline()
  skip_on_cran()

  cum_welfare <- datt_rural$L
  cum_population <- datt_rural$p

  # Correct retrieval of Lorenz curve data points
  lorenz <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
                   estimate = "lorenz", n_bins = 10)

  expect_s3_class(lorenz, "data.frame")
  expect_true(all(c("weight", "welfare") %in% names(lorenz)))
  expect_equal(nrow(lorenz), 10)

  # Handling of lorenz methodology
  lorenz_lb <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
                      estimate = "lorenz", lorenz = "lb", n_bins = 100)

  expect_s3_class(lorenz_lb, "data.frame")
  expect_true(all(c("weight", "welfare") %in% names(lorenz_lb)))

  lorenz_lq <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population,
                      estimate = "lorenz", lorenz = "lq", n_bins = 100)

  expect_s3_class(lorenz_lq, "data.frame")
  expect_true(all(c("weight", "welfare") %in% names(lorenz_lq)))
})

### Section 4: Testing 'params' Endpoint -----
test_that("'params' Endpoint Tests", {
  skip_if_offline()
  skip_on_cran()

  cum_welfare <- datt_rural$L
  cum_population <- datt_rural$p

  # Proper retrieval of regression parameters
  params <- get_gd(cum_welfare = cum_welfare, cum_population = cum_population, estimate = "params")

  expect_s3_class(params, "data.frame")
  expect_true(all(c("A", "B") %in% names(params))) # Check some example parameter names
})

### Section 5: Additional  -----
test_that("Edge Cases and Error Handling", {
  skip_if_offline()
  skip_on_cran()

  # Both cum_welfare and cum_population missing
  expect_error(
    get_gd(estimate = "stats", requested_mean = 19, povline = 2.15)
  )

  # Invalid lorenz method
  expect_error(
    get_gd(cum_welfare = datt_rural$L, cum_population = datt_rural$p,
           estimate = "lorenz", lorenz = "invalid_method", n_bins = 10),
    "You supplied an invalid value for lorenz"
  )
})
