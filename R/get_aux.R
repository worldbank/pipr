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
#' \dontrun{
#' # Get list of tables
#' x <- get_aux()
#'
#' # Get GDP data
#' df <- get_aux("gdp")
#'
#' # Get countries
#' df <- get_aux("countries")
#' }
get_aux <- function(table = NULL, version = NULL, api_version = "v1",
                    format = c("rds", "json", "csv"),
                    simplify = TRUE, server = NULL) {

  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Check connection
  check_internet()
  check_api(api_version, server)

  # Build query string
  u <- build_url(server, "aux", api_version = api_version)

  # Return response
  if (is.null(table)) {
    res <- httr::GET(u)
    parse_response(res, simplify = simplify)
  } else {
    args <- build_args(table = table, version = version, format = format)
    res <- httr::GET(u, query = args, httr::user_agent(pipr_user_agent))
    parse_response(res, simplify = simplify)
  }
}

#' get_countries
#' @description Returns a table countries with their full names, ISO codes, and
#' associated region code
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get countries
#' get_countries()
#' }
get_countries <- function(version = NULL, api_version = "v1",
                          format = c("rds", "json", "csv"),
                          server = NULL) {
  get_aux("countries",
    version = version, api_version = api_version,
    format = format, server = server
  )
}


#' get_regions
#' @description Returns a table regional grouping used for computing aggregate
#' poverty statistics.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get regions
#' get_regions()
#' }
get_regions <- function(version = NULL, api_version = "v1",
                        format = c("rds", "json", "csv"),
                        server = NULL) {
  get_aux("regions",
    version = version, api_version = api_version,
    format = format, server = server
  )
}


#' get_cpi()
#' @description Returns a table of Consumer Price Index (CPI) values used for
#' poverty and inequality computations.
#' statistics
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get cpi
#' get_cpi()
#' }
get_cpi <- function(version = NULL, api_version = "v1",
                                 format = c("rds", "json", "csv"),
                                 server = NULL) {
  get_aux("cpi",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_dictionary
#' @description Returns a data dictionary with a description of all variables
#' available through the PIP API.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get dictionary
#' get_dictionary()
#' }
get_dictionary <- function(version = NULL, api_version = "v1",
                              format = c("rds", "json", "csv"),
                              server = NULL) {
  get_aux("dictionary",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_gdp()
#' @description Returns a table of Growth Domestic Product (GDP) values used for
#' poverty and inequality
#' statistics
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get gdp
#' get_gdp()
#' }
get_gdp <- function(version = NULL, api_version = "v1",
                          format = c("rds", "json", "csv"),
                          server = NULL) {
  get_aux("gdp",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_incgrp_coverage
#' @description Returns a table of survey coverage for low and lower-middle
#' income countries. If this coverage is less than 50%, World level aggregate
#' statistics will not be computed.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get incgrp_coverage
#' get_incgrp_coverage()
#' }
get_incgrp_coverage <- function(version = NULL, api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("incgrp_coverage",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_interpolated_means
#' @description Returns a table of key information and statistics for all years
#' for which poverty and inequality statistics are either available (household
#' survey exists) or extra- / interpolated.
#' Please see \code{\link{get_dictionary}} for more information about
#' each variable available in this table.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get interpolated_means
#' get_interpolated_means()
#' }
get_interpolated_means <- function(version = NULL, api_version = "v1",
                           format = c("rds", "json", "csv"),
                           server = NULL) {
  get_aux("interpolated_means",
          version = version, api_version = api_version,
          format = format, server = server
  )
}

#' get_hfce
#'
#' @description Returns a table of Household Final Consumption Expenditure (HFCE) values
#' used for poverty and inequality computations
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get hfce
#' get_hfce()
#' }
get_hfce <- function(version = NULL, api_version = "v1",
                                   format = c("rds", "json", "csv"),
                                   server = NULL) {
  get_aux("pce",
          version = version, api_version = api_version,
          format = format, server = server
  )
}

#' get_pop
#' @description Returns a table of population values used for poverty and
#' inequality computations.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get pop
#' get_pop()
#' }
get_pop <- function(version = NULL, api_version = "v1",
                                   format = c("rds", "json", "csv"),
                                   server = NULL) {
  get_aux("pop",
          version = version, api_version = api_version,
          format = format, server = server
  )
}

#' get_pop_region
#' @description Returns a table of total population by region-year. These values
#' are used for the computation of regional aggregate poverty statistics.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get pop_region
#' get_pop_region()
#' }
get_pop_region <- function(version = NULL, api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("pop_region",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_ppp
#' @description Returns a table of Purchasing Power Parity (PPP) values
#' used for poverty and inequality computations.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get ppp
#' get_ppp()
#' }
get_ppp <- function(version = NULL, api_version = "v1",
                              format = c("rds", "json", "csv"),
                              server = NULL) {
  get_aux("ppp",
          version = version, api_version = api_version,
          format = format, server = server
  )
}

#' get_region_coverage
#' @description Return a table of regional survey coverage: Percentage of
#' available surveys for a specific region-year.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get region_coverage
#' get_region_coverage()
#' }
get_region_coverage <- function(version = NULL, api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("region_coverage",
          version = version, api_version = api_version,
          format = format, server = server
  )
}


#' get_survey_means
#' @description Returns a table of all available surveys and associated key
#' statistics. Please see \code{\link{get_dictionary}} for more information about
#' each variable available in this table.
#' @export
#' @examples
#' \dontrun{
#' # Short hand to get survey_means
#' get_survey_means()
#' }
get_survey_means <- function(version = NULL, api_version = "v1",
                                format = c("rds", "json", "csv"),
                                server = NULL) {
  get_aux("survey_means",
          version = version, api_version = api_version,
          format = format, server = server
  )
}
