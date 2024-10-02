#' Build request version 2
#'
#' @param server character: Server. For WB internal use only
#' @param api_version character: API version
#' @param endpoint character: PIP API endpoint
#' @param ...
#'
#' @return httr2 request
#'
build_request_v2 <- function(server,
                             api_version,
                             endpoint,
                             ...) {



  base_url <- select_base_url(server = server)
  params <- list(...)

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_url_path_append(endpoint) |>
    # .multi = "comma" works fine without applying fix_params
    httr2::req_url_query(!!!params, .multi = "comma") |>
    httr2::req_cache(tools::R_user_dir("pipr", which = "cache"),
                     use_on_error = TRUE,
                     debug = TRUE) |>
    httr2::req_user_agent(pipr_user_agent) |>
    httr2::req_error(body = parse_error_body) |>
    httr2::req_retry(
      is_transient = pip_is_transient,
      after = retry_after,
      max_seconds = 60
    )


  return(req)

}

