#' Check API status
#' @inheritParams get_stats
#' @return character
#' @export
#' @examples
#' health_check()
health_check <- function(api_version = "v1", server = NULL) {
  check_internet()
  res <- check_api(api_version, server = server)
  parse_response(res, simplify = FALSE)$content
}

#' Get versions
#'
#' Get available data versions.
#' @inheritParams get_stats
#' @return character
#' @export
#' @examples
#' get_versions()
get_versions <- function(api_version = "v1", server = NULL) {
  check_internet()
  u <- build_url(server, "versions", api_version)
  res <- httr::GET(u)
  parse_response(res, simplify = FALSE)$content
}

#' Get PIP info
#'
#' Get information about the API.
#' @inheritParams get_stats
#' @return list
#' @export
#' @examples
#' get_pip_info()
get_pip_info <- function(api_version = "v1", server = NULL) {
  check_internet()
  u <- build_url(server, "pip-info", api_version)
  res <- httr::GET(u)
  parse_response(res, simplify = FALSE)$content
}
