#' Check API status
#' @inheritParams get_stats
#' @export
#' @examples
#' health_check()
health_check <- function(api_version = "v1"){
  res <- check_api(api_version)
  parse_response(res, simplify = FALSE)$content
}

#' Get countries
#'
#' @return tibble
#' @export
#' @examples
#' get_countries()
get_countries <- function(){
  get_aux("countries", api_version = api_version)

}

#' Get regions
#'
#' @return tibble
#' @export
#' @examples
#' get_regions()
get_regions <- function(api_version = "v1"){
  get_aux("regions", api_version = api_version)
}

#' Get AUX table
#'
#' Get an auxiliary table. If no table is specified it a vector with possible
#' inputs will be returned.
#'
#' @param table Aux table
#' @return tibble
#' @export
#' @examples
#' # Get list of tables
#' x <- get_aux()
#'
#' # Get GDP data
#' df <- get_aux("gdp")
#'
get_aux <- function(table = NULL, api_version = "v1") {
  u <- build_url(base_url, "aux", api_version = api_version)
  if (is.null(table)) {
    res <- httr::GET(u)
    parse_response(res, simplify = FALSE)$content
  } else {
    res <- httr::GET(u, query = list(table = table))
    parse_response(res, simplify = TRUE)
  }
}
