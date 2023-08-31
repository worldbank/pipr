#' build_request
#'
#' @param country
#' @param year
#' @param povline
#' @param popshare
#' @param fill_gaps
#' @param group_by
#' @param welfare_type
#' @param reporting_level
#' @param version
#' @param ppp_version
#' @param release_version
#' @param format
#' @param server
#'
#' @return httr2 request
#'
build_request <- function(country,
                          year,
                          povline,
                          popshare,
                          fill_gaps,
                          group_by,
                          welfare_type,
                          reporting_level,
                          version,
                          ppp_version,
                          release_version,
                          format,
                          server) {

  base_url <- select_base_url(server = server)

  params <- list(
    country         = country,
    year            = year,
    povline         = povline,
    popshare        = popshare,
    fill_gaps       = fill_gaps,
    group_by        = group_by,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    version         = version,
    ppp_version     = ppp_version,
    release_version = release_version,
    format          = format
  )

  req <- httr2::request(base_url) |>
    httr2::req_url_path_append(api_version) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_user_agent(pipr_user_agent) |>
    httr2::req_retry(
      is_transient = pip_is_transient,
      after = retry_after,
      max_seconds = 60
    )# |>
    #httr2::req_cache(tempdir(), use_on_error = TRUE)

  return(req)

}
