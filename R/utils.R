#' check_internet
#' @noRd
check_internet <- function() {
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connexion")
  invisible(TRUE)
}

#' check_api
#' @inheritParams get_stats
#' @noRd
check_api <- function(api_version, server = NULL) {
  u <- build_url(server, "health-check", api_version)
  res <- httr::GET(u)
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = "Could not connect to the API"
  )
  invisible(res)
}

#' check_status
#' @param res A httr response
#' @param parsed A parsed response
#' @noRd
check_status <- function(res, parsed) {
  if (res$status_code != 200) {
    if ("error" %in% names(parsed)) {
      msg1 <- paste(
        httr::http_status(res$status_code)$message,
        parsed$error,
        "Use simplify = FALSE to see the full error response.",
        sep = "\n*\t")
    } else {
      msg1 <- paste(
        httr::http_status(res$status_code)$message,
        "Use simplify = FALSE to see the full error response.",
        sep = "\n*\t")
    }
    attempt::stop_if_not(
      .x = httr::status_code(res),
      .p = ~ .x == 200,
      msg = msg1
    )
  }
  invisible(TRUE)
}

#' check_host
#' @inheritParams send_query
#' @return logical
#' @noRd
check_host <- function(server, ...)  {
  base_url <- select_base_url(server)
  host <- gsub("/pip|/api|http(s)?://", "", base_url)
  retry_host(host, ...)
  invisible(TRUE)
}

#' Retry host
#'
#' Retry connection to a server host in case the host could not be resolved.
#'
#' @param host A server host
#' @param times Maximum number of requests to attempt
#' @param min Minimum number of seconds to sleep for each retry
#' @param max Maximum number of seconds to sleep for each retry
#' @return logical
#' @noRd
#' @examples
#' retry_host("google.com")
#' retry_host("google.tmp")
#' @importFrom stats runif
retry_host <- function(host, times = 3L, min = 1, max = 3) {
  # Only do one request of times == 1
  if (times == 1)  {
    check <- curl::nslookup(host, error = FALSE)
  } else {
    # Else iterate over n times
    for (i in seq_len(times)) {
      check <- curl::nslookup(host, error = FALSE)
      if (!is.null(check)) break
      sleep <- round(runif(1, min, max), 1)
      message(sprintf("Could not connect to %s. Retrying in %s seconds...", host, sleep))
      Sys.sleep(sleep)
    }
  }
  attempt::stop_if(is.null(check), msg = sprintf("Could not connect to %s", host))
  invisible(TRUE)
}

#' Retry request
#'
#' Retry a GET request in case the server returns a 500 type error.
#'
#' @param url A URL
#' @param query Query parameters (optional)
#' @param times Maximum number of requests to attempt
#' @param min Minimum number of seconds to sleep for each retry
#' @param max Maximum number of seconds to sleep for each retry
#' @return A httr response
#' @noRd
#' @examples
#' retry_request("http://httpbin.org/status/200")
#' retry_request("http://httpbin.org/status/400")
#' retry_request("http://httpbin.org/status/500")
retry_request <- function(url, query = NULL, times = 3L, min = 1, max = 3) {
  # Only do one request if times == 1
  if (times == 1)  {
    res <- httr::GET(url, query = query, httr::user_agent(pipr_user_agent))
    return(res)
  }
  # Iterate over n times
  for (i in seq_len(times)) {
    res <- httr::GET(url, query = query, httr::user_agent(pipr_user_agent))
    if (!res$status_code %in% c(429, 500, 503, 504)) break
    sleep <- round(runif(1, min, max), 1)
    message(sprintf("Request failed [%s]. Retrying in %s seconds...", res$status_code, sleep))
    Sys.sleep(sleep)
  }
  return(res)
}

#' build_url
#' @param server character: Server. Either "prod", "qa" or "dev". Defaults to
#'   NULL (ie. prod).
#' @param endpoint character: Endpoint
#' @param api_version character: API version
#' @inheritParams get_stats
#' @noRd
build_url <- function(server, endpoint, api_version) {
  base_url <- select_base_url(server = server)
  sprintf("%s/%s/%s", base_url, api_version, endpoint)
}

#' Select base URL
#'
#' Helper function to switch base URLs depending on PIP server being used
#'
#' @inheritParams build_url
#' @return character
#' @noRd
select_base_url <- function(server) {

  if (!is.null(server)) {
    match.arg(server, c("prod", "qa", "dev"))
    # Check ENV vars for DEV/QA urls
    if (server %in% c("qa", "dev")) {
      if (server == "qa") base_url <- Sys.getenv("PIP_QA_URL")
      if (server == "dev") base_url <- Sys.getenv("PIP_DEV_URL")
      attempt::stop_if(
        base_url == "",
        msg = sprintf("'%s' url not found. Check your .Renviron file.", server)
      )
    }
  }

  # Set base_url to prod_url (standard)
  if (is.null(server) || server == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}

#' build_args
#' @inheritParams get_stats
#' @noRd
build_args <- function(country = NULL,
                       year = NULL,
                       povline = NULL,
                       popshare = NULL,
                       fill_gaps = NULL,
                       group_by = NULL,
                       welfare_type = NULL,
                       reporting_level = NULL,
                       table = NULL,
                       version = NULL,
                       format = NULL) {

  # Collapse to a single string
  if (length(country) > 1) country <- paste0(country, collapse = ",")
  if (length(year) > 1) year <- paste0(year, collapse = ",")

  args <- list(
    country = country, year = year, povline = povline,
    popshare = popshare, fill_gaps = fill_gaps,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    group_by = group_by, table = table,
    version = version, format = format
  )
  attempt::stop_if_all(args, is.null, "You need to specify at least one argument")

  args <- purrr::compact(args)

  return(args)
}

#' Send API query
#'
#' @inheritParams build_url
#' @inheritParams query Query parameters (optional)
#' @param ... Additional parameters passed to `retry_host()` and
#'   `retry_request()`
#' @return A httr response
#' @noRd
send_query <- function(server, query = NULL, endpoint, api_version, ...) {
  # check_host(server, ...)
  u <- build_url(server, endpoint, api_version)
  retry_request(u, query = query, ...)
}


#' parse_response
#' @param res A httr response
#' @inheritParams get_stats
#' @noRd
parse_response <- function(res, simplify) {

  # Get response type
  type <- tryCatch(suppressWarnings(httr::http_type(res)), error = function(e) NULL)

  # Stop if response type is unknown
  attempt::stop_if(is.null(type), msg = "Invalid response format")

  if (type == "application/json") {
    parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  }
  if (type == "text/csv") {
    parsed <- suppressMessages(httr::content(res, encoding = "UTF-8"))
  }
  if (type == "application/rds") {
    parsed <- unserialize(res$content)
  }

  if (simplify) {
    check_status(res, parsed)
    parsed <- tibble::as_tibble(parsed)
    # TEMP fix for renaming of columns
    # To be removed when pipapi#207
    # has been implemented
    parsed <- tmp_rename_cols(parsed)
    # TEMP fix END
    return(parsed)
  } else {
    structure(
      list(
        url = res$url,
        status = res$status_code,
        type = type,
        content = parsed,
        response = res
      ),
      class = "pip_api"
    )
  }
}

#' Select base URL
#'
#' Helper function to switch base URLs depending on PIP server being used
#'
#' @param server character: c("prod", "qa", "dev"). Defaults to NULL (ie. prod).
#' @return character
#' @noRd
select_base_url <- function(server) {
  if (!is.null(server)) {
    match.arg(server, c("prod", "qa", "dev"))
    if (server %in% c("qa", "dev")) {
      if (server == "qa") base_url <- Sys.getenv("PIP_QA_URL")
      if (server == "dev") base_url <- Sys.getenv("PIP_DEV_URL")
      attempt::stop_if(
        base_url == "",
        msg = sprintf("'%s' url not found. Check your .Renviron file.", server)
      )
    }
  }

  if (is.null(server) || server == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}

#' Rename columns
#' TEMP function to rename response cols
#' @param df A data.frame
#' @noRd
tmp_rename_cols <- function(df) {
  df <- data.table::setnames(
    df,
    old = c("survey_year", "reporting_year", "reporting_pop", "reporting_gdp", "reporting_pce", "pce_data_level"),
    new = c("welfare_time", "year", "pop", "gdp", "hfce", "hfce_data_level"),
    skip_absent = TRUE
  )
  return(df)
}
