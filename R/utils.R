base_url <- "https://api.worldbank.org/pip"

#' check_internet
#' @noRd
check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connexion")
}

#' check_api
#' @inheritParams get_stats
#' @noRd
check_api <- function(api_version){
  u <- build_url(base_url, "health-check", api_version)
  res <- httr::GET(u)
  attempt::stop_if_not(.x = httr::status_code(res),
              .p = ~ .x == 200,
              msg = "Could not connect to the API")
  invisible(res)
}

#' check_status
#' @param res A httr response
#' @noRd
check_status <- function(res){
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = "The API returned an error")
}

#' build_url
#' @param base_url Base URL
#' @param endpoint Endpoint
#' @inheritParams get_stats
#' @noRd
build_url <- function(base_url, endpoint, api_version){
  sprintf("%s/%s/%s", base_url, api_version, endpoint)
}

#' build_args
#' @inheritParams get_stats
#' @noRd
build_args <- function(
  country = NULL,
  year = NULL,
  povline = NULL,
  popshare = NULL,
  fill_gaps = NULL,
  group_by = NULL,
  welfare_type = NULL,
  reporting_level = NULL,
  version = NULL,
  format = NULL,
  table = NULL) {

  args <- list(country = country, year = year, povline = povline,
               fill_gaps = fill_gaps, welfare_type = welfare_type,
               reporting_level = reporting_level, version = version,
               format = format, group_by = group_by, table = table)
  attempt::stop_if_all(args, is.null, "You need to specify at least one argument")

  args <- purrr::compact(args)

  return(args)

}

#' parse_response
#' @param res A httr response
#' @inheritParams get_stats
#' @noRd
parse_response <- function(res, simplify) {

  type <- httr::http_type(res)
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
