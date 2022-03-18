base_url <- "https://api.worldbank.org/pip"

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
    msg1 <- httr::http_status(res$status_code)$message
    attempt::stop_if_not(
      .x = httr::status_code(res),
      .p = ~ .x == 200,
      msg = msg1
    )
  }
  msg2 <- "Invalid query parameters have been submitted"
  attempt::stop_if(parsed == msg2, msg = msg2)
  invisible(TRUE)
}

#' build_url
#' @param server Server
#' @param endpoint Endpoint
#' @inheritParams get_stats
#' @noRd
build_url <- function(server, endpoint, api_version) {
  if (!is.null(server)) {
    match.arg(server, c("prod", "qa", "dev"))
    if (server == "qa") base_url <- Sys.getenv("PIP_QA_URL")
    if (server == "dev") base_url <- Sys.getenv("PIP_DEV_URL")
    attempt::stop_if(
      base_url == "",
      msg = sprintf("'%s' url not found. Check your .Renviron file.", server)
    )
  }
  if (is.null(server) || server == "prod") base_url <- base_url
  sprintf("%s/%s/%s", base_url, api_version, endpoint)
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
