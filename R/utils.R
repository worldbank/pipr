#' check_internet
#' @noRd
check_internet <- function() {
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connection")
  invisible(TRUE)
}

#' health_check
#' @inheritParams check_api
#' @noRd
health_check <- function(api_version, server = NULL) {
  u <- build_base_url(server, "health-check", api_version)
  res <- httr::GET(u)
  attempt::stop_if_not(
    .x = httr::status_code(res),
    .p = ~ .x == 200,
    msg = "Could not connect to the API"
  )
  invisible(res)
}

#' check_status
#' @param res A httr response
#' @param parsed A parsed response
#' @noRd
check_status <- function(res, parsed) {
  if (res$status_code != 200) {
    if ("error" %in% names(parsed)) {
      msg1 <- paste(
        httr::http_status(res$status_code)$message,
        parsed$error,
        "Use simplify = FALSE to see the full error response.",
        sep = "\n*\t")
    } else {
      msg1 <- paste(
        httr::http_status(res$status_code)$message,
        "Use simplify = FALSE to see the full error response.",
        sep = "\n*\t")
    }
    attempt::stop_if_not(
      .x = httr::status_code(res),
      .p = ~ .x == 200,
      msg = msg1
    )
  }
  invisible(TRUE)
}

#' build_base_url
#' @param server character: Server
#' @param endpoint character: Endpoint
#' @param api_version character: API version
#' @inheritParams get_stats
#' @noRd
build_base_url <- function(server, endpoint, api_version) {
  base_url <- select_base_url(server = server)
  sprintf("%s/%s/%s", base_url, api_version, endpoint)
}

#' build_args
#' @inheritParams get_stats
#' @noRd
build_args <- function(.country = NULL,
                       .year = NULL,
                       .povline = NULL,
                       .popshare = NULL,
                       .fill_gaps = NULL,
                       .group_by = NULL,
                       .welfare_type = NULL,
                       .reporting_level = NULL,
                       .table = NULL,
                       .version = NULL,
                       .ppp_version = NULL,
                       .release_version = NULL,
                       .format = NULL) {

  # Collapse to a single string
  if (length(.country) > 1) .country <- paste0(.country, collapse = ",")
  if (length(.year) > 1) .year <- paste0(.year, collapse = ",")

  args <- list(
    country = .country,
    year = .year,
    povline = .povline,
    popshare = .popshare,
    fill_gaps = .fill_gaps,
    welfare_type = .welfare_type,
    reporting_level = .reporting_level,
    group_by = .group_by,
    table = .table,
    version = .version,
    ppp_version = .ppp_version,
    release_version = .release_version,
    format = .format
  )

  attempt::stop_if_all(args, is.null, "You need to specify at least one argument")

  args <- purrr::compact(args)

  return(args)
}


#' parse_response
#' @param res A httr response
#' @inheritParams get_stats
#'
#' @return If `simplify = TRUE`, it returns a tibble with the requested content.
#'   If `simplify = FALSE`, it returns a list of class "pip_api" with
#'   information about the PIP API query
#' @keywords internal
parse_response <- function(res, simplify) {

  # Get response type
  type <- tryCatch(suppressWarnings(httr2::resp_content_type(res)), error = function(e) NULL)

  # Stop if response type is unknown
  attempt::stop_if(is.null(type), msg = "Invalid response format")

  if (type == "application/json") {
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(res, encoding = "UTF-8"))
  }
  if (type == "text/csv") {
    parsed <- suppressMessages(httr2::resp_body_string(res, encoding = "UTF-8"))
  }
  if (type == "application/rds") {
    parsed <- unserialize(res$body)
  }

  if (simplify) {
    check_status(res, parsed)
    parsed <- tibble::as_tibble(parsed)
    # TEMP fix for renaming of columns
    # To be removed when pipapi#207
    # has been implemented
    parsed <- tmp_rename_cols(parsed, res$url)
    # TEMP fix END
    return(parsed)
  } else {
    structure(
      list(
        url = res$url,
        status = res$status_code,
        type = type,
        content = parsed,
        response = res
      ),
      class = "pip_api"
    )
  }
}

#' Select base URL
#'
#' Helper function to switch base URLs depending on PIP server being used
#'
#' @param server character: c("prod", "qa", "dev"). Defaults to NULL (ie. prod).
#' @return character
#' @noRd
select_base_url <- function(server) {
  if (!is.null(server)) {
    match.arg(server, c("prod", "qa", "dev"))
    if (server %in% c("qa", "dev")) {
      if (server == "qa") base_url <- Sys.getenv("PIP_QA_URL")
      if (server == "dev") base_url <- Sys.getenv("PIP_DEV_URL")
      attempt::stop_if(
        base_url == "",
        msg = sprintf("'%s' url not found. Check your .Renviron file.", server)
      )
    }
  }

  if (is.null(server) || server == "prod") {
    base_url <- prod_url
  }

  return(base_url)
}

#' Rename columns
#' TEMP function to rename response cols
#' @param df A data.frame
#' @param url response url
#' @noRd
tmp_rename_cols <- function(df, url = "") {
    df <- data.table::setnames(
      df,
      old = c("survey_year", "reporting_year", "reporting_pop", "reporting_gdp", "reporting_pce", "pce_data_level"),
      new = c("welfare_time", "year", "pop", "gdp", "hfce", "hfce_data_level"),
      skip_absent = TRUE
    )

  return(df)
}

#' pip_is_transient
#'
#' Helper function to determine if an error is due to the number of requests
#' going over the rate limit
#'
#' @param resp
#'
#' @return logical
#'
pip_is_transient <- function(resp) {
  if (httr2::resp_is_error(resp)) {
  httr2::resp_status(resp) == 429 &
      stringr::str_detect(httr2::resp_body_json(resp, check_type = FALSE)$message,
                        "Rate limit is exceeded")
  } else {
    FALSE
  }
}

retry_after <- function(resp) {
  if (httr2::resp_is_error(resp)) {
    time <- httr2::resp_body_json(resp, check_type = FALSE)$message
    time <- stringr::str_remove(time, "Rate limit is exceeded. Try again in ")
    readr::parse_number(time)
  } else {
    0
  }
}
