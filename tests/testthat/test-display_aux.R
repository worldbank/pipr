test_that("returns proper table", {
  skip_if_offline()
  skip_on_cran()
  api_version <-  "v1"
  simplify    <-  TRUE
  server      <-  NULL
  u           <- build_url(server, "aux", api_version = api_version)
  res         <- httr::GET(u)
  tbs_tb      <- parse_response(res, simplify = simplify)

  tt <- suppressMessages(display_aux(server = server, simplify = simplify, api_version = api_version))

  expect_equal(tbs_tb, tt)

})
