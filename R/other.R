#' Check API status
#' @inheritParams get_stats
#' @return character
#' @export
#' @examples
#' health_check()
health_check <- function(api_version = "v1", server = NULL) {
  res <- send_query(server, endpoint = "health-check", api_version = api_version)
  parse_response(res, simplify = FALSE)$content
}

#' Get versions
#'
#' Get available data versions.
#' @inheritParams get_stats
#' @return tibble or list
#' @export
#' @examples
#' get_versions()
get_versions <- function(api_version = "v1", server = NULL, simplify = TRUE) {
  res <- send_query(server, endpoint = "versions", api_version = api_version)
  parse_response(res, simplify = simplify)
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
  res <- send_query(server, endpoint = "pip-info", api_version = api_version)
  parse_response(res, simplify = FALSE)$content
}
