#' Get auxiliary data
#'
#' @description `get_aux()` Get an auxiliary dataset. If no table is specified a
#'   vector with possible inputs will be returned.
#'
#'   `get_countries()` Returns a table countries with their full names, ISO
#'   codes, and associated region code
#'
#'
#' @param table Aux table
#' @param assign_tb assigns table to specified name to the `.pip` environment.
#'   If `FALSE` no assignment will performed. If `TRUE`, the table will be
#'   assigned to  exactly the same name as the one of the desired table. If
#'   character, the table will be assigned to that name.
#' @inheritParams get_stats
#' @param replace logical: force replacement of aux files in `.pip` env. Default
#'   is FALSE.
#'
#' @return If `simplify = FALSE`, it returns a list of class "pip_api". If
#'   `simplify = TRUE`, it returns a tibble with the requested data. This is the
#'   default. Only for `get_aux()`, If `assign_tb = TRUE` or character, it
#'   returns TRUE when data was assign properly to .pip env. FALSE, if it was
#'   not assigned.
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
#'
#' # Display auxiliary tables
#' get_aux()
#'
#' # Display and assign to .pip env the selected auxiliary table
#' get_aux(assign_tb = TRUE)
#'
#' # Bind gdp table to "gdp" in .pip env
#' get_aux("gdp", assign_tb = TRUE)
#'
#' # Bind gdp table to "new_name" in .pip env
#' get_aux("gdp", assign_tb = "new_name")
#'
#' }
get_aux <- function(table           = NULL,
                    version         = NULL,
                    ppp_version     = NULL,
                    release_version = NULL,
                    api_version     = "v1",
                    format          = c("rds", "json", "csv"),
                    simplify        = TRUE,
                    server          = NULL,
                    assign_tb       = FALSE,
                    replace         = FALSE) {

  # Match args
  api_version <- match.arg(api_version)
  format      <- match.arg(format)
  run_cli     <- run_cli()
  # Build query string
  req <- build_request(server = server,
                       api_version = api_version,
                       endpoint = "aux")

  # Return response
  # If no table is specified, returns list of available tables
  if (is.null(table)) {
    res <- req |>
      httr2::req_perform()
    tables <- parse_response(res, simplify = simplify)
    cli::cli_text("Auxiliary tables available are")
    cli::cli_ul(tables$tables)
    if (run_cli) {
      cltxt <- paste0("You can type {.run pipr::display_aux()} to display a
                      clickable list of available
                      auxiliary tables")

      cli::cli_alert_info(cltxt, wrap = TRUE)
    }
    return(invisible(tables))
  # If a table is specified, returns that table
  } else {
    req <- build_request(server          = server,
                         api_version     = api_version,
                         endpoint        = "aux",
                         table           = table,
                         version         = version,
                         release_version = release_version,
                         format          = format)

    res <- httr2::req_perform(req)
    rt  <- parse_response(res, simplify = simplify)
  }

  # Should the table be saved in a dedicated environment for later retrieval?
  if (!isFALSE(assign_tb)) {
    # If not FALSE. It could be TRUE or character
    # YES: Assign fetched tables to dedicated environment
    if (isTRUE(assign_tb)) {
      tb_name <- table

    } else if (is.character(assign_tb)) {
      tb_name <- assign_tb

    } else {
      msg <- c("Invalid syntax in {.field assign_tb}",
               "*" = "{.field assign_tb} must be logical or character.")
        cli::cli_abort(msg, wrap = TRUE)
    }

    srt <- set_aux(table = tb_name,
                   value = rt,
                   replace = replace)

    if (isTRUE(srt)) {

      cltxt <- paste0("Auxiliary table {.strong {table}} successfully fetched. ",
                      "You can now call it by typing {.",
                      ifelse(run_cli, "run", "code"),
                      " pipr::call_aux(", shQuote(tb_name), ")}")

      cli::cli_alert_info(cltxt, wrap = TRUE)

      return(invisible(srt))

    } else {

      msg <- c("table {.strong {table}} could not be saved in env {.env .pip}")
      cli::cli_abort(msg, wrap = TRUE)

    }

  } else {
    # NO: Just return the table
    return(rt)
  }

}

#' @describeIn get_aux Returns a table countries with their full names, ISO
#'   codes, and associated region code
#' @examples
#' \dontrun{
#' # Short hand to get countries
#' get_countries()
#' }
get_countries <- function(version = NULL,
                          ppp_version = NULL,
                          release_version = NULL,
                          api_version = "v1",
                          format = c("rds", "json", "csv"),
                          server = NULL) {
  get_aux("countries",
    version = version,
    ppp_version = ppp_version,
    release_version = release_version,
    api_version = api_version,
    format = format, server = server
  )
}


#' @describeIn get_aux Returns a table regional grouping used for computing
#'   aggregate poverty statistics.
#' @examples
#' \dontrun{
#' # Short hand to get regions
#' get_regions()
#' }
get_regions <- function(version = NULL,
                        ppp_version = NULL,
                        release_version = NULL,
                        api_version = "v1",
                        format = c("rds", "json", "csv"),
                        server = NULL) {
  get_aux("regions",
    version = version,
    ppp_version = ppp_version,
    release_version = release_version,
    api_version = api_version,
    format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of Consumer Price Index (CPI) values used
#'   for poverty and inequality computations. statistics
#' @examples
#' \dontrun{
#' # Short hand to get cpi
#' get_cpi()
#' }
get_cpi <- function(version = NULL,
                    ppp_version = NULL,
                    release_version = NULL,
                    api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("cpi",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a data dictionary with a description of all
#'   variables available through the PIP API.
#' @examples
#' \dontrun{
#' # Short hand to get dictionary
#' get_dictionary()
#' }
get_dictionary <- function(version = NULL,
                           ppp_version = NULL,
                           release_version = NULL,
                           api_version = "v1",
                           format = c("rds", "json", "csv"),
                           server = NULL) {
  get_aux("dictionary",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of Growth Domestic Product (GDP) values
#'   used for poverty and inequality statistics.
#' @examples
#' \dontrun{
#' # Short hand to get gdp
#' get_gdp()
#' }
get_gdp <- function(version = NULL,
                    ppp_version = NULL,
                    release_version = NULL,
                    api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("gdp",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of survey coverage for low and
#'   lower-middle income countries. If this coverage is less than 50%, World
#'   level aggregate statistics will not be computed.
#' @examples
#' \dontrun{
#' # Short hand to get incgrp_coverage
#' get_incgrp_coverage()
#' }
get_incgrp_coverage <- function(version = NULL,
                                ppp_version = NULL,
                                release_version = NULL,
                                api_version = "v1",
                                format = c("rds", "json", "csv"),
                                server = NULL) {
  get_aux("incgrp_coverage",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of key information and statistics for all
#'   years for which poverty and inequality statistics are either available
#'   (household survey exists) or extra- / interpolated. Please see
#'   \code{\link{get_dictionary}} for more information about each variable
#'   available in this table.
#' @examples
#' \dontrun{
#' # Short hand to get interpolated_means
#' get_interpolated_means()
#' }
get_interpolated_means <- function(version = NULL,
                                   ppp_version = NULL,
                                   release_version = NULL,
                                   api_version = "v1",
                                   format = c("rds", "json", "csv"),
                                   server = NULL) {
  get_aux("interpolated_means",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}

#' @describeIn get_aux Returns a table of Household Final Consumption
#'   Expenditure (HFCE) values used for poverty and inequality computations.
#' @examples
#' \dontrun{
#' # Short hand to get hfce
#' get_hfce()
#' }
get_hfce <- function(version = NULL,
                     ppp_version = NULL,
                     release_version = NULL,
                     api_version = "v1",
                     format = c("rds", "json", "csv"),
                     server = NULL) {
  get_aux("pce",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}

#' @describeIn get_aux Returns a table of population values used for poverty and
#' inequality computations.
#' @examples
#' \dontrun{
#' # Short hand to get pop
#' get_pop()
#' }
get_pop <- function(version = NULL,
                    ppp_version = NULL,
                    release_version = NULL,
                    api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("pop",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}

#' @describeIn get_aux Returns a table of total population by region-year. These
#'   values are used for the computation of regional aggregate poverty
#'   statistics.
#' @examples
#' \dontrun{
#' # Short hand to get pop_region
#' get_pop_region()
#' }
get_pop_region <- function(version = NULL,
                           ppp_version = NULL,
                           release_version = NULL,
                           api_version = "v1",
                           format = c("rds", "json", "csv"),
                           server = NULL) {
  get_aux("pop_region",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of Purchasing Power Parity (PPP) values
#'   used for poverty and inequality computations.
#' @examples
#' \dontrun{
#' # Short hand to get ppp
#' get_ppp()
#' }
get_ppp <- function(version = NULL,
                    ppp_version = NULL,
                    release_version = NULL,
                    api_version = "v1",
                    format = c("rds", "json", "csv"),
                    server = NULL) {
  get_aux("ppp",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}

#' @describeIn get_aux Return a table of regional survey coverage: Percentage of
#'   available surveys for a specific region-year.
#' @examples
#' \dontrun{
#' # Short hand to get region_coverage
#' get_region_coverage()
#' }
get_region_coverage <- function(version = NULL,
                                ppp_version = NULL,
                                release_version = NULL,
                                api_version = "v1",
                                format = c("rds", "json", "csv"),
                                server = NULL) {
  get_aux("region_coverage",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}


#' @describeIn get_aux Returns a table of all available surveys and associated
#'   key statistics. Please see \code{\link{get_dictionary}} for more
#'   information about each variable available in this table.
#' @examples
#' \dontrun{
#' # Short hand to get survey_means
#' get_survey_means()
#' }
get_survey_means <- function(version = NULL,
                             ppp_version = NULL,
                             release_version = NULL,
                             api_version = "v1",
                             format = c("rds", "json", "csv"),
                             server = NULL) {
  get_aux("survey_means",
          version = version,
          ppp_version = ppp_version,
          release_version = release_version,
          api_version = api_version,
          format = format, server = server
  )
}
