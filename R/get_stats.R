#' Get poverty and inequality statistics
#'
#' @param country character: A vector with one or more \href{https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm}{country ISO 3 codes} or
#'   'all'
#' @param year integer: A vector with one or more years or 'all'
#' @param povline numeric: Poverty line
#' @param popshare numeric: Proportion of the population living below the
#'   poverty line
#' @param fill_gaps logical: If TRUE, will interpolate / extrapolate values for
#'   missing years
#' @param nowcast logical: If TRUE, will return nowcast estimates.
#' @param subgroup character: If used result will be aggregated for predefined
#'   sub-groups. Either 'wb_regions' or 'none'.
#' @param welfare_type character: Welfare type either of c("all", "income", "consumption")
#' @param reporting_level character: Geographical reporting level either of c("all", "national", "urban", "rural")
#' @param version character: Data version. See `get_versions()`
#' @param ppp_version ppp year to be used
#' @param release_version date when the data was published in YYYYMMDD format
#' @param api_version character: API version
#' @param format character: Response format either of c("rds", "json", "csv")
#' @param simplify logical: If TRUE (the default) the response is returned as a
#'   `tibble`
#' @param server character: Server. For WB internal use only
#'
#' @return If `simplify = FALSE`, it returns a list of class "pip_api". If
#'   `simplify = TRUE`, it returns a tibble with the requested data. This is the
#'   default. Only for `get_aux()`, If `assign_tb = TRUE` or character, it
#'   returns TRUE when data was assign properly to .pip env. FALSE, if it was
#'   not assigned.
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
#' res <- get_stats("all", year = "all", subgroup = "wb")
#'
#' # Short hand to get WB global/regional stats
#' res <- get_wb()
#' 
#' # Short hand to get fcv stats
#' res <- get_agg(aggregate = "fcv", server = "qa")
#'
#' # Custom aggregates
#' res <- get_stats(c("ARG", "BRA"), year = "all", subgroup = "none")
#' }
get_stats <- function(country = "all",
                      year = "all",
                      povline = NULL,
                      popshare = NULL,
                      fill_gaps = FALSE,
                      nowcast = FALSE,
                      subgroup = NULL,
                      welfare_type = c("all", "income", "consumption"),
                      reporting_level = c("all", "national", "urban", "rural"),
                      version = NULL,
                      ppp_version = NULL,
                      release_version = NULL,
                      api_version = "v1",
                      format = c("arrow", "rds", "json", "csv"),
                      simplify = TRUE,
                      server = NULL) {
  # Match args
  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # popshare can't be used together with povline
  if (!is.null(popshare)) povline <- NULL

  # nowcast = TRUE -> fill_gaps = TRUE
  if (nowcast) fill_gaps <- TRUE

  # otherwise we cannot filter correctly because estimate_type not returned
  if (isFALSE(fill_gaps)) nowcast <- FALSE

  # subgroup can't be used together with fill_gaps
  if (!is.null(subgroup)) {
    fill_gaps <- NULL # subgroup can't be used together with fill_gaps
    nowcast <- NULL # assuming this is the same for nowcast
    endpoint <- "pip-grp"
    subgroup <- match.arg(subgroup, c("none", "wb_regions"))
    if (subgroup == "wb_regions") {
      group_by <- "wb"
    } else {
      group_by <- subgroup
    }
  } else {
    endpoint <- "pip"
    group_by <- NULL
  }


  # Build query string
  req <- build_request(
    country         = country,
    year            = year,
    povline         = povline,
    popshare        = popshare,
    fill_gaps       = fill_gaps,
    nowcast         = nowcast,
    group_by        = group_by,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = endpoint
  )

  # Perform request
  res <- req |>
    httr2::req_perform()

  # Parse result
  out <- parse_response(res, simplify)

  # Filter nowcast
  ## (only when simplify == TRUE) because filtering happens after the request is returned.
  if ( !is.null(nowcast) & isFALSE(nowcast) & simplify == TRUE) {
    out <- out[!grepl("nowcast", out$estimate_type),]
  }

  return(out)
}

#' @rdname get_stats
#' @export
get_wb <- function(year = "all",
                   povline = NULL,
                   version = NULL,
                   ppp_version = NULL,
                   release_version = NULL,
                   api_version = "v1",
                   format = c("rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {

  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Build query string
  req <- build_request(
    year            = year,
    povline         = povline,
    group_by        = "wb",
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = "pip-grp"
  )
  # Perform request
  res <- req |>
    httr2::req_perform()

  # Parse result
  out <- parse_response(res, simplify)

  return(out)
}

#' @rdname get_stats
#' @param aggregate character: Aggregate name. See `get_aux("countries")` for available options.
#' @export
get_agg <- function(year = "all",
                   povline = NULL,
                   version = NULL,
                   ppp_version = NULL,
                   release_version = NULL,
                   aggregate = NULL,
                   api_version = "v1",
                   format = c("rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {

  # Match args
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # Extract varibale name that don't have "_code" and "_name" suffixes from countries auxiliary table
  df <- get_aux("countries")
  ctry_vars <- names(df)[!grepl("_code$|_name$", names(df))]
  ctry_vars <- c("official", "pcn", "vintage", ctry_vars)

  # Validate aggregate
  if (!is.null(aggregate)) {

    aggregate <- tolower(aggregate)
    if (!(aggregate %in% ctry_vars)) {
      cli::cli_abort(
      c(
        "Invalid aggregate name.",
        "x" = "Please use one of the following: {.val {ctry_vars}}"
      )
      )
    } else if (aggregate %in% c("official", "region", "world")) {

      agg <- "wb"

    } else if (aggregate %in% c("pcn", "vintage", "regionpcn")) {

      agg <- "vintage"

    } else {

      agg <- aggregate

    }

  } else {

    cli::cli_abort(
      c(
        "Aggregate name is required.",
        "i" = "Please use one of the following: {.val {ctry_vars}}"
      )
    )
  }

  # Build query string
  req <- build_request(
    year            = year,
    povline         = povline,
    group_by        = agg,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = "pip-grp"
  )
  # Perform request
  res <- req |>
    httr2::req_perform()

  # Parse result
  out <- parse_response(res, simplify)

  return(out)
}
