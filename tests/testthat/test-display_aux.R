test_that("returns proper table", {
  skip_if_offline()
  skip_on_cran()
  api_version <-  "v1"
  simplify    <-  TRUE
  server      <-  NULL
  req         <- build_request(server      = server,
                               api_version = api_version,
                               endpoint    = "aux")
  res         <- httr2::req_perform(req)
  tbs_tb      <- parse_response(res, simplify = simplify)

  tt <- suppressMessages(display_aux(server = server, simplify = simplify, api_version = api_version))

  expect_equal(tbs_tb, tt)

})
