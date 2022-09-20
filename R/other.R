#' Check internet connection and API status
#' @inheritParams get_stats
#' @return character
#' @export
#' @examples
#' \dontrun{
#' check_api()
#' }
check_api <- function(api_version = "v1", server = NULL) {
  check_internet()
  res <- health_check(api_version, server = server)
  parse_response(res, simplify = FALSE)$content
}

#' Get versions
#'
#' Get available data versions.
#' @inheritParams get_stats
#' @return tibble or list
#' @export
#' @examples
#' \dontrun{
#' get_versions()
#' }
get_versions <- function(api_version = "v1", server = NULL, simplify = TRUE) {
  check_internet()
  u <- build_url(server, "versions", api_version)
  res <- httr::GET(u, httr::user_agent(pipr_user_agent))
  parse_response(res, simplify = simplify)
}

#' Get PIP info
#'
#' Get information about the API.
#' @inheritParams get_stats
#' @return list
#' @export
#' @examples
#' \dontrun{
#' get_pip_info()
#' }
get_pip_info <- function(api_version = "v1", server = NULL) {
  check_internet()
  u <- build_url(server, "pip-info", api_version)
  res <- httr::GET(u, httr::user_agent(pipr_user_agent))
  parse_response(res, simplify = FALSE)$content
}
