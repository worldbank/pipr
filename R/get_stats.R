#' Get poverty and inequality statistics
#'
#' @param country character: A vector with one or more country ISO 3 codes or
#'   'all'
#' @param year integer: A vector with one or more years or 'all'
#' @param povline numeric: Poverty line
#' @param popshare numeric: Proportion of the population living below the
#'   poverty line
#' @param fill_gaps logical: If TRUE, will interpolate / extrapolate values for
#'   missing years
#' @param group_by character: If used result will be aggregated for predefined
#'   sub-groups
#' @param welfare_type character: Welfare type
#' @param reporting_level character: Geographical reporting level
#' @param version character: Data version. See `get_versions()`
#' @param api_version character: API version
#' @param format character: Response format
#' @param simplify logical: If TRUE (default) the response is converted to a
#'   data frame
#' @param server character: Server. For WB internal use only
#'
#' @return tibble or list
#' @export
#'
#' @examples
#' \dontrun{
#' # One country-year
#' res <- get_stats(country = "AGO", year = 2000)
#'
#' # All years for a specific country
#' res <- get_stats(country = "AGO", year = "all")
#'
#' # All countries and years
#' res <- get_stats(country = "all", year = "all")
#'
#' # All countries and years w/ alternative poverty line
#' res <- get_stats(country = "all", year = "all", povline = 3.2)
#'
#' # Fill gaps for years without available survey data
#' res <- get_stats(country = "all", year = "all", fill_gaps = TRUE)
#'
#' # Proportion living below the poverty line
#' res <- get_stats(country = "all", year = "all", popshare = .4)
#'
#' # World Bank global and regional aggregates
#' get_stats("all", year = "all", group_by = "wb")
#'
#' # Custom aggregates
#' get_stats(c("ARG", "BRA"), year = "all", group_by = "none")
#' }
get_stats <- function(country = "all",
                      year = "all",
                      povline = 1.9,
                      popshare = NULL,
                      fill_gaps = FALSE,
                      group_by = NULL,
                      welfare_type = c("all", "income", "consumption"),
                      reporting_level = c("all", "national", "urban", "rural"),
                      version = NULL,
                      api_version = "v1",
                      format = c("json", "csv", "rds"),
                      simplify = TRUE,
                      server = NULL) {

  # Match args
  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # popshare can't be used together with povline
  if (!is.null(popshare)) povline <- NULL

  if (!is.null(group_by)) {
    fill_gaps <- NULL # group_by can't be used together with fill_gaps
    endpoint <- "pip-grp"
    group_by <- match.arg(group_by, c("none", "wb"))
  } else {
    endpoint <- "pip"
  }

  # Check connection
  check_internet()
  check_api(api_version, server)

  # Build query string
  args <- build_args(
    country = country, year = year, povline = povline,
    fill_gaps = fill_gaps, group_by = group_by,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    version = version, format = format,
  )
  u <- build_url(server, endpoint, api_version)

  # Send query
  res <- httr::GET(u, query = args)

  # Parse result
  out <- parse_response(res, simplify)

  return(out)
}