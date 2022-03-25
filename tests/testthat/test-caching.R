library(callr)

test_that("Caching is enabled by default", {
  # Setup external R session
  r <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
  r$run(function() library(pipr))
  # Check that main functions are cached
  tmp <- r$run(function() memoise::is.memoised(get_stats))
  expect_true(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_wb))
  expect_true(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_aux))
  expect_true(tmp)
  r$kill()
})

test_that("Caching can be disabled", {
  # Setup external R session
  r <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
  r$run(function() Sys.setenv("PIPR_DISABLE_CACHING" = "TRUE"))
  r$run(function() library(pipr))
  # Check that main functions are NOT cached
  tmp <- r$run(function() memoise::is.memoised(get_stats))
  expect_false(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_wb))
  expect_false(tmp)
  tmp <- r$run(function() memoise::is.memoised(get_aux))
  expect_false(tmp)
  r$kill()
})
