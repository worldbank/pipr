



get_cp <- function(country = "all",
                   povline = NULL,
                   ppp_version = NULL,
                   api_version = "v1",
                   format = c("rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {


  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Build query string
  req <- build_request(
    country         = country,
    povline         = povline,
    ppp_version     = ppp_version,
    format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = "cp-download"
  )

  # Perform request
  res <- req |>
    httr2::req_perform()

  # Parse result
  out <- parse_response(res, simplify)

  return(out)

}
