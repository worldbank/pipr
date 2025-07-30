
#' Get grouped stats
#'
#' Get grouped stats from the PIP API.
#' @inheritParams get_stats
#' @param cum_welfare numeric: Cumulative welfare values, expressed in shares. Any length. They should be monotonically increasing, and sum to 1.
#' @param cum_population numeric: Cumulative population values, expressed in shares. Any length. They should be monotonically increasing, and sum to 1.
#' @param estimate character: One of "stats", "lorenz", "params".
#' @param requested_mean numeric: Requested mean.
#' @param povline numeric: Poverty line. Required for estimate = "stats".
#' @param lorenz character: Lorenz curve methodology. Either "lb" or "lq".
#' @param n_bins numeric: Number of bins. Required for estimate = "lorenz".
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' datt_data <- data.frame(p = c(0.0092, 0.0339, 0.0850, 0.160, 0.2609, 0.4133,
#'                               0.5497, 0.7196, 0.8196, 0.9174, 0.9570, 0.9751,
#'                               1),
#'                        L = c(0.00208, 0.001013, 0.03122, 0.07083, 0.12808,
#'                              0.23498, 0.34887, 051994, 0.64270, 0.79201,
#'                              0.86966, 0.91277, 1)
#'
#' # estimate = 'stats': retrieve poverty statistics.
#' stats <- get_gd(cum_welfare = datt_data$L, cum_population = datt_data$p,
#'                 estimate = "stats",
#'                 requested_mean = 19, # default is 1.
#'                 povline = 2.15)  # default is 1.
#'
#' # estimate = 'lorenz': retrieve Lorenz curve data points for a specified number of bins.
#'
#' ## Best lorenz curve methodolody selected by default:
#' lorenz <- get_gd(cum_welfare = datt_data$L,
#'                  cum_population = datt_data$p,
#'                  estimate = "lorenz",
#'                  n_bins = 100)  # must be specified, default is NULL.
#'
#' ## Specify lorenz curve methodology:
#' ### Beta Lorenz ("lb")
#' lorenz_lb <- get_gd(cum_welfare = datt_data$L,
#'                  cum_population = datt_data$p,
#'                  estimate = "lorenz",
#'                  lorenz = "lb",
#'                  n_bins = 100)
#'
#' ### Quadratic Lorenz ("lq")
#' lorenz_lq <- get_gd(cum_welfare = datt_data$L,
#'                  cum_population = datt_data$p,
#'                  estimate = "lorenz",
#'                  lorenz = "lq",
#'                  n_bins = 100)
#'
#' # estimate = 'params': retrieve regression parameters used for the lorenz curve estimation.
#' params <- get_gd(cum_welfare = datt_data$L,
#'                  cum_population = datt_data$p,
#'                  estimate = "params")
#'}

get_gd <- function(cum_welfare,
                   cum_population,
                   estimate = c("stats", "lorenz", "params"),
                   requested_mean = NULL, # stats specific.
                   povline = NULL, # stats specific.
                   popshare = NULL, # stats specific.
                   lorenz = NULL, # lorenz specific.
                   n_bins = NULL, # lorenz specific.
                   api_version = "v1",
                   format = c("rds", "json", "csv"),
                   simplify = TRUE,
                   server = NULL) {

  # 0. Match args -------------------------------------------------------------
  estimate <- match.arg(estimate)
  api_version <- match.arg(api_version)
  format <- match.arg(format)

  # 0. General Args checks (Error is general otherwise) -----------------------
  if (length(cum_welfare) != length(cum_population)) {
    cli::cli_abort("{.val cum_welfare} and {.val cum_population} must have the same length.")
  }

  # 1. endpoint = grouped-stats ------------------------------------------------
    if (estimate == "stats") {

      endpoint <- "grouped-stats"

      # 1.1 grouped-stats args checks ------
      if (is.null(requested_mean)) {
        cli::cli_abort("For endpoint {endpoint}, {.val requested_mean} must be provided.")
      }
      if (is.null(povline) && is.null(popshare)) {
        cli::cli_abort("For endpoint {.field endpoint}, you must provide either {.arg povline} or {.arg popshare} argument")
      }

      # popshare can't be used together with povline
      if (!is.null(popshare)) povline <- NULL

      # 1.2 Build request for grouped-stats ------
      req <- build_request(
        cum_welfare     = cum_welfare,
        cum_population  = cum_population,
        requested_mean  = requested_mean,
        povline         = povline,
        popshare        = popshare,
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

    }

  # 2. endpoint = lorenz-curve ------------------------------------------------
    if (estimate == "lorenz") {

      endpoint <- "lorenz-curve"

      # 2.1 Build request for lorenz-curve ------
      req <- build_request(
        cum_welfare     = cum_welfare,
        cum_population  = cum_population,
        lorenz          = lorenz,
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

    }

  # 3. endpoint = regress-params -----------------------------------------------
    if (estimate == "params") {

      endpoint <- "regression-params"

      # 3.2 Build request for regress-params
      req <- build_request(
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

    }

    return(out)

}
