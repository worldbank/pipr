#' Get Country Profiles
#'
#' @inheritParams get_stats
#' @param povline One finite, non-negative numeric poverty line, or `NULL`.
#'   With `ppp_version = 2011`, `NULL` sends 1.9; otherwise the API applies
#'   its default poverty line.
#' @param format Response format: `"arrow"`, `"rds"`, `"json"`, or `"csv"`.
#'
#' @return A tibble of country profile data when `simplify = TRUE`, or a
#'   `pip_api` list when `simplify = FALSE`.
#' @export
#'
#' @examples
#' if (interactive()) {
#' # One country, all years with default ppp_version = 2017
#' res <- get_cp(country = "AGO")
#'
#' # All countries, povline = 1.9
#' res <- get_cp(povline = 1.9)
#'
#' # All countries and years with default values
#' res <- get_cp()
#' }
get_cp <- function(country = "all",
                   povline = 2.15, # GC: default value like Stata
                   version = NULL,
                   ppp_version = 2017, # GC: default value like Stata
                   release_version = NULL,
                   api_version = "v1",
                   format = c("arrow", "rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {


  # 0. Match args ----
  validated_args <- validate_get_cp_args(
    country         = country,
    povline         = povline,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    api_version     = api_version,
    format          = format,
    simplify        = simplify,
    server          = server
  )
  api_version <- validated_args$api_version
  format <- validated_args$format

  # 1. povline set-up ----
  # (GC: stata equivalent but no 2005 and default to 2.15)
  if (is.null(povline)) {
    if (ppp_version == "2011") {
      povline <- 1.9
    }
  }


  # 2. Build query string ----
  req <- build_request(
    country         = country,
    povline         = povline,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = "cp-download"
  )


  # 3. Perform request ----
  res <- req |>
    httr2::req_perform()

  # 4. Parse result and return
  out <- parse_response(res, simplify)

  return(out)

}
