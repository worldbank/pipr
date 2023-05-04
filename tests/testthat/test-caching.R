library(callr)

test_that("Caching is enabled by default", {
  skip_on_cran()
  # Setup external R session
  r <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
  # r$run(function() Sys.setenv("PIPR_DISABLE_CACHING" = "FALSE"))
  r$run(function() library(pipr))
  # Check that main functions are cached

  ## get_stats ------
  r$run(function() get_stats()) |>
    pipr:::is_cached(df = _) |>
    expect_null()


  r$run(function() get_stats()) |>
    pipr:::is_cached(df = _) |>
    expect_true()

  ## get_wb ------
  r$run(function() get_wb()) |>
    pipr:::is_cached(df = _) |>
    expect_null()


  r$run(function() get_wb()) |>
    pipr:::is_cached(df = _) |>
    expect_true()

  ## get_aux ------
  r$run(function() get_aux(table = "countries")) |>
    pipr:::is_cached(df = _) |>
    expect_null()


  r$run(function() get_aux(table = "countries")) |>
    pipr:::is_cached(df = _) |>
    expect_true()


  r$kill()
})
