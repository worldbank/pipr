#' Get Country Profiles Key Indicators
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
#' # One country, all years with default ppp_version = 2017
#' res <- get_cp(country = "IDN")
#'
#' # All countries, povline = 1.9
#' res <- get_cp(country = "IDN", povline = 1.9)
#'
#' }
get_cp_ki <- function(country = NULL,
                      povline = 2.15, # GC: default value like Stata
                      version = NULL,
                      ppp_version = 2017, # GC: default value like Stata
                      release_version = NULL,
                      api_version = "v1",
                      #format = c("arrow", "rds", "json", "csv"),
                      simplify = TRUE,
                      server = NULL) {


  # 0. Match args ----
  api_version <- match.arg(api_version)
  #format <- match.arg(format)

  # 1. povline set-up ----
  # (GC: stata equivalent but no 2005 and default to 2.15)
  if (is.null(povline)) {
    if (ppp_version == "2011") {
      povline <- 1.9
    }
  }

  # 2. country set-up ----
  if (is.null(country)) {
    cli::cli_abort("Please provide a country code.")
  }

  if (length(country) > 1) {
    cli::cli_abort("Please provide only one country code.")
  }


  # 2. Build query string ----
  req <- build_request(
    country         = country,
    povline         = povline,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    #format          = format,
    server          = server,
    api_version     = api_version,
    endpoint        = "cp-key-indicators"
  )


  # 3. Perform request ----
  res <- req |>
    httr2::req_perform()

  # 4. Parse result and return (if simplify == FALSE)
  if (isFALSE(simplify)) {
    out <- parse_response(res, simplify)
  } else {
    out <- parse_response(res, simplify)

    # 5. Unnest
    out <- unnest_ki(out)

  }

  return(out)

}


# Unnest key indicators  ----
#' Unnest the key indicators
#' @describeIn unnest_ki takes the simplified output from cp-key-indicators endpoint and unnests it.
#'
#' @param out parsed and simplified output from cp-key-indicators endpoint
#'
#' @return data frame, unnested.
#'
unnest_ki <- function(out){

  # Step 1: Extract all data frames into individual variables
  headcount <- out$headcount[[1]]
  headcount_national <- out$headcount_national[[1]]
  mpm_headcount <- out$mpm_headcount[[1]]
  pop <- out$pop[[1]]

  gni <- out$gni[[1]]
  gni <- gni[!duplicated(gni[c("country_code", "reporting_year")]), ]


  gdp_growth <- out$gdp_growth[[1]]
  gdp_growth <- gdp_growth[!duplicated(gdp_growth[c("country_code", "reporting_year")]), ]

  shared_prosperity <- out$shared_prosperity[[1]]

  # Step 2: Merge data frames on common columns
  df_merged <- merge(headcount, headcount_national, by = c("country_code", "reporting_year"), all = TRUE)
  df_merged <- merge(df_merged, mpm_headcount, by = c("country_code", "reporting_year"), all = TRUE)
  df_merged <- merge(df_merged, pop, by = c("country_code", "reporting_year"), all = TRUE)
  df_merged <- merge(df_merged, gni, by = c("country_code", "reporting_year"), all = TRUE)
  df_merged <- merge(df_merged, gdp_growth, by = c("country_code", "reporting_year"), all = TRUE, suffixes = c("_gni", "_gdp"))
  final_df <- merge(df_merged, shared_prosperity, by = "country_code", all = TRUE)

  # Step 3: return
  return(final_df)

}

