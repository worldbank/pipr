#' Get auxiliary data
#'
#' @description `get_aux()` gets an auxiliary dataset. If no table is specified,
#'   it lists the available tables.
#'
#'   Use `get_aux("countries")` for a table of country names, ISO codes,
#'   and associated region codes.
#'
#'
#' @param table Aux table
#' @param assign_tb assigns table to specified name to the `.pip` environment.
#'   If `FALSE` no assignment will performed. If `TRUE`, the table will be
#'   assigned to  exactly the same name as the one of the desired table. If
#'   character, the table will be assigned to that name.
#' @inheritParams get_stats
#' @param format Response format: `"rds"`, `"json"`, or `"csv"`.
#' @param ppp_version Validated as a PPP year, but not sent to the auxiliary
#'   endpoint because this endpoint does not accept it.
#' @param replace logical: force replacement of aux files in `.pip` env. Default
#'   is FALSE.
#'
#' @return With no `table`, invisibly returns the available tables (a tibble
#'   with a `tables` column by default). With a selected table, returns its
#'   tibble when `simplify = TRUE`, or a `pip_api` list when `simplify = FALSE`.
#'   With `assign_tb = TRUE` or a name, invisibly returns `TRUE` on successful
#'   assignment to `.pip`; unsuccessful assignment raises an error.
#' @export
#' @examples
#' if (interactive()) {
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
  validated_args <- validate_get_aux_args(
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    api_version     = api_version,
    format          = format,
    simplify        = simplify,
    server          = server
  )
  api_version <- validated_args$api_version
  format      <- validated_args$format
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
    # ppp_version is validated but not sent: the aux endpoint does not accept it
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
#' if (interactive()) {
#' # Short hand to get countries
#' get_aux("countries")
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
#' if (interactive()) {
#' # Short hand to get regions
#' get_aux("regions")
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
#' if (interactive()) {
#' # Short hand to get cpi
#' get_aux("cpi")
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
#' if (interactive()) {
#' # Short hand to get dictionary
#' get_aux("dictionary")
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
#' if (interactive()) {
#' # Short hand to get gdp
#' get_aux("gdp")
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
#' if (interactive()) {
#' # Short hand to get incgrp_coverage
#' get_aux("incgrp_coverage")
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
#'   `get_aux("dictionary")` for more information about each variable in this
#'   table.
#' @examples
#' if (interactive()) {
#' # Short hand to get interpolated_means
#' get_aux("interpolated_means")
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
#' if (interactive()) {
#' # Short hand to get hfce
#' get_aux("pce")
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
#' if (interactive()) {
#' # Short hand to get pop
#' get_aux("pop")
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
#' if (interactive()) {
#' # Short hand to get pop_region
#' get_aux("pop_region")
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
#' if (interactive()) {
#' # Short hand to get ppp
#' get_aux("ppp")
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
#' if (interactive()) {
#' # Short hand to get region_coverage
#' get_aux("region_coverage")
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
#'   key statistics. See `get_aux("dictionary")` for more information about
#'   each variable in this table.
#' @examples
#' if (interactive()) {
#' # Short hand to get survey_means
#' get_aux("survey_means")
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
