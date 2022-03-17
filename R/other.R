#' Check API status
#' @inheritParams get_stats
#' @export
#' @examples
#' health_check()
health_check <- function(api_version = "v1", server = NULL) {
  check_internet()
  res <- check_api(api_version, server = server)
  parse_response(res, simplify = FALSE)$content
}

#' Get countries
#'
#' Get a dataset with all countries used in PIP.
#'
#' @inheritParams get_stats
#' @return tibble
#' @export
#' @examples
#' get_countries()
get_countries <- function(version = NULL, api_version = "v1",
                          format = c("json", "csv", "rds"),
                          server = NULL) {
  get_aux("countries",
    version = version, api_version = api_version,
    format = format, server = server
  )
}

#' Get regions
#'
#' Get a dataset with all regions used in PIP.
#'
#' @inheritParams get_stats
#' @return tibble
#' @export
#' @examples
#' get_regions()
get_regions <- function(version = NULL, api_version = "v1",
                        format = c("json", "csv", "rds"),
                        server = NULL) {
  get_aux("regions",
    version = version, api_version = api_version,
    format = format, server = server
  )
}

#' Get AUX table
#'
#' Get an auxiliary table. If no table is specified it a vector with possible
#' inputs will be returned.
#'
#' @param table Aux table
#' @inheritParams get_stats
#' @return tibble or character vector
#' @export
#' @examples
#' # Get list of tables
#' x <- get_aux()
#'
#' # Get GDP data
#' df <- get_aux("gdp")
get_aux <- function(table = NULL, version = NULL, api_version = "v1",
                    format = c("json", "csv", "rds"),
                    simplify = TRUE, server = NULL) {

  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Check connection
  check_internet()
  check_api(api_version, server)

  # Build query string
  u <- build_url(server, "aux", api_version = api_version)

  # Get available tables
  res <- httr::GET(u)
  tables <- parse_response(res, simplify = FALSE)$content

  # Check table input
  if (!is.null(table)) {
    attempt::stop_if_not(
      table %in% tables,
      msg = sprintf("'%s' is not an available table", table)
    )
  }

  # Return response
  if (is.null(table)) {
    # res <- httr::GET(u)
    # parse_response(res, simplify = FALSE)$content
    return(tables)
  } else {
    args <- build_args(table = table, version = version, format = format)
    res <- httr::GET(u, query = args)
    parse_response(res, simplify = simplify)
  }
}
