#' Get poverty and inequality statistics
#'
#' @param country Uppercase, three-letter country ISO codes or `"all"`.
#' @param year Integer-valued numeric years, `"all"`, or `"MRV"` (most recent
#'   value). Character strings containing years are not accepted.
#' @param povline One finite, non-negative numeric poverty line, or `NULL`.
#'   Ignored when `popshare` is supplied.
#' @param popshare One numeric population share from 0 to 1, or `NULL`.
#' @param fill_gaps One non-missing logical value. If `TRUE`, interpolate or
#'   extrapolate missing years. Also enabled by `nowcast = TRUE`.
#' @param nowcast One non-missing logical value. If `TRUE`, return nowcast
#'   estimates and enable `fill_gaps`.
#' @param subgroup `NULL`, `"wb_regions"`, or `"none"`. A non-`NULL` value uses
#'   the grouped endpoint and disables gap filling and nowcasting.
#' @param welfare_type character: Welfare type either of c("all", "income", "consumption")
#' @param reporting_level character: Geographical reporting level either of c("all", "national", "urban", "rural")
#' @param version Data version in `YYYYMMDD_PPP_XX_YY_IDENTITY` format, or
#'   `NULL`. See `get_versions()`.
#' @param ppp_version One integer-valued numeric PPP year, a four-digit year
#'   string, or `NULL`.
#' @param release_version A valid publication date in `YYYYMMDD` format, or
#'   `NULL`.
#' @param api_version API version. Currently only `"v1"` is supported.
#' @param format Response format: `"arrow"`, `"rds"`, `"json"`, or `"csv"` for
#'   `get_stats()`. `get_wb()` and `get_agg()` accept `"rds"`, `"json"`, or
#'   `"csv"`.
#' @param simplify One non-missing logical value. If `TRUE` (the default),
#'   return simplified data.
#' @param server One non-empty server name, or `NULL` for the production server.
#'   For World Bank internal use only.
#'
#' @return A tibble of statistics when `simplify = TRUE`, or a `pip_api` list
#'   when `simplify = FALSE`.
#' @export
#'
#' @examples
#' if (interactive()) {
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
#' res <- get_stats("all", year = "all", subgroup = "wb_regions")
#'
#' # Short hand to get WB global/regional stats
#' res <- get_wb()
#' 
#' # Short hand to get fcv stats
#' res <- get_agg(aggregate = "fcv")
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
  validated_args <- validate_get_stats_args(
    country = country,
    year = year,
    povline = povline,
    popshare = popshare,
    fill_gaps = fill_gaps,
    nowcast = nowcast,
    subgroup = subgroup,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    version = version,
    ppp_version = ppp_version,
    release_version = release_version,
    api_version = api_version,
    format = format,
    simplify = simplify,
    server = server
  )
  welfare_type <- validated_args$welfare_type
  reporting_level <- validated_args$reporting_level
  api_version <- validated_args$api_version
  format <- validated_args$format

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
#' @param aggregate Aggregate name. See `get_aux("country_list")` for available
#'   options.
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
  df <- get_aux("country_list")
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
