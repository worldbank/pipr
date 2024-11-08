#' Get Country Profiles
#'
#' @inheritParams get_stats
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
#' res <- get_cp(country = "AGO")
#'
#' # All countries and years
#' res <- get_cp()
#' }
get_cp <- function(country = "all",
                   povline = NULL,
                   version = NULL,
                   ppp_version = 2017, # we need to give a default value
                   release_version = NULL,
                   api_version = "v1",
                   format = c("arrow", "rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {


  # 0. Match args ----
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # 1. povline set-up ----
  # (GC: stata equivalent but no 2005 and default to 2.15)
  if (is.null(povline)) {
    if (ppp_version == "2011") {
      povline <- 1.9
    } else {
      povline <- 2.15
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
