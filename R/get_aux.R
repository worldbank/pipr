#' Get auxiliary data
#'
#' Get an auxiliary dataset. If no table is specified a vector with possible
#' inputs will be returned.
#'
#' @param table Aux table
#' @inheritParams get_stats
#' @return tibble or list
#' @export
#' @examples
#' # Get list of tables
#' x <- get_aux()
#'
#' # Get GDP data
#' df <- get_aux("gdp")
#'
#' # Get countries
#' df <- get_aux("countries")
get_aux <- function(table = NULL, version = NULL, api_version = "v1",
                    format = c("rds", "json", "csv"),
                    simplify = TRUE, server = NULL) {

  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Return response
  if (is.null(table)) {
    res <- send_query(server, endpoint = "aux", api_version = api_version)
    parse_response(res, simplify = simplify)
  } else {
    args <- build_args(table = table, version = version, format = format)
    res <-  send_query(server, endpoint = "aux",
                       query = args,
                       api_version = api_version)
    parse_response(res, simplify = simplify)
  }
}

#' @rdname get_aux
#' @export
#' @examples
#'
#' # Short hand to get countries
#' get_countries()
get_countries <- function(version = NULL, api_version = "v1",
                          format = c("rds", "json", "csv"),
                          server = NULL) {
  get_aux("countries",
    version = version, api_version = api_version,
    format = format, server = server
  )
}

#' @rdname get_aux
#' @export
#' @examples
#'
#' # Short hand to get regions
#' get_regions()
get_regions <- function(version = NULL, api_version = "v1",
                        format = c("rds", "json", "csv"),
                        server = NULL) {
  get_aux("regions",
    version = version, api_version = api_version,
    format = format, server = server
  )
}
