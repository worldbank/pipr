
#' Get grouped stats
#'
#' Get grouped stats from the PIP API.
#' @inheritParams get_stats
#' @param cum_welfare numeric: Cumulative welfare values.
#' @param cum_population numeric: Cumulative population values.
#' @param endpoint character: One of "grouped-stats", "lorenz-curve", "regression-params".
#' @param requested_mean numeric: Requested mean.
#' @param povline numeric: Poverty line. Required for endpoint = "grouped-stats".
#' @param n_bins numeric: Number of bins. Required for endpoint = "lorenz-curve".

get_grouped_stats <- function(cum_welfare =  NULL,
                              cum_population = NULL,
                              endpoint = c("grouped-stats", "lorenz-curve", "regression-params"),
                              requested_mean = NULL, # grouped-stats specific
                              povline = NULL, # grouped-stats specific
                              #lorenz = NULL, # lorenz-curve specific (not working for now)
                              n_bins = NULL, # lorenz-curve specific
                              api_version = "v1",
                              format = c("rds", "json", "csv"), # arrow does not work.
                              simplify = TRUE,
                              server = NULL) {

  # 0. Match args -------------------------------------------------------------
  endpoint <- match.arg(endpoint)
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # 0. General Args checks (Error is general otherwise) -----------------------
  if (length(cum_welfare) != length(cum_population)) {
    cli::cli_abort("{.val cum_welfare} and {.val cum_population} must have the same length.")
  }

  # 1. endpoint = grouped-stats ------------------------------------------------
    if (endpoint == "grouped-stats") {

      # 1.1 grouped-stats args checks ------
      if (is.null(requested_mean)) {
        cli::cli_abort("For endpoint {endpoint}, {.val requested_mean} must be provided.")
      }
      if (is.null(povline)) {
        cli::cli_abort("For endpoint {endpoint}, {.val povline} must be provided.")
      }


      # 1.2 Build request for grouped-stats ------
      req <- build_request_v2(
        cum_welfare     = cum_welfare,
        cum_population  = cum_population,
        requested_mean  = requested_mean,
        povline         = povline,
        format          = format,
        server          = server,
        api_version     = api_version,
        endpoint        = endpoint
      )

      # 1.3 Perform request ------
      res <- req |>
        httr2::req_perform()

      # 1.4 Parse result ------
      out <- parse_response(res, simplify)

      # 1.5 Return ------
      return(out)
    }

  # 2. endpoint = lorenz-curve ------------------------------------------------
    if (endpoint == "lorenz-curve") {


      # 2.1 Build request for lorenz-curve ------
      req <- build_request_v2(
        cum_welfare     = cum_welfare,
        cum_population  = cum_population,
        #lorenz          = lorenz,
        n_bins          = n_bins,
        format          = format,
        server          = server,
        api_version     = api_version,
        endpoint        = endpoint
      )

      # 2.2 Perform request ------
      res <- req |>
        httr2::req_perform()

      # 2.3 Parse result ------
      out <- parse_response(res, simplify)

      # 2.4 Return ------
      return(out)
    }

  # 3. endpoint = regress-params -----------------------------------------------
    if (endpoint == "regression-params") {


      # 3.2 Build request for regress-params
      req <- build_request_v2(
        cum_welfare     = cum_welfare,
        cum_population  = cum_population,
        format          = format,
        server          = server,
        api_version     = api_version,
        endpoint        = endpoint
      )

      # 3.3 Perform request ------
      res <- req |>
        httr2::req_perform()

      # 3.4 Parse result ------
      out <- parse_response(res, simplify)

      # 3.5 Return ------
      return(out)
    }
}
