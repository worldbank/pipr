#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parsing and checking functions  -------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PR 63

#' check_internet
#' @noRd
check_internet <- function() {
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connection")
  invisible(TRUE)
}

#' health_check
#' @inheritParams check_api
#' @noRd
health_check <- function(api_version = "v1", server = NULL) {
  req <- build_request(server      = server,
                       api_version = api_version,
                       endpoint    = "health-check")
  res <- httr2::req_perform(req)
  attempt::stop_if_not(
    .x = httr2::resp_status(res),
    .p = ~ .x == 200,
    msg = "Could not connect to the API"
  )
  invisible(res)
}

#' check_status
#' @param res A httr response
#' @param parsed A parsed response
#' @noRd
check_status <- function(res) {
  if (httr2::resp_is_error(res)) {
      msg1 <- paste(
        httr2::resp_status_desc(res),
        "Use simplify = FALSE to see the full error response.",
        sep = "\n*\t")

    attempt::stop_if_not(
      .x = httr2::resp_status(res),
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

  # Classify the response type
  res_health <- FALSE
  if (grepl("health-check", res$url)) {
    res_health <- TRUE
  }

  pip_info <- FALSE
  if (grepl("pip-info", res$url)) {
    pip_info <- TRUE
  }

  # Get response type
  type <- tryCatch(suppressWarnings(httr2::resp_content_type(res)), error = function(e) NULL)

  # Stop if response type is unknown
  attempt::stop_if(is.null(type), msg = "Invalid response format")

  if (type == "application/vnd.apache.arrow.file") {
    parsed <- arrow::read_feather(res$body)
    # GC: right now arrow not working with grouped-stats, so I won't pivot.
  }

  if (type == "application/json") {

    if (res_health | pip_info) {
      parsed <- jsonlite::fromJSON(httr2::resp_body_string(res, encoding = "UTF-8"))
    } else {
    parsed <- jsonlite::fromJSON(httr2::resp_body_string(res, encoding = "UTF-8"))
    parsed <- change_grouped_stats_to_csv(parsed) # GC: used to pivot.
    }
  }

  if (type == "text/csv") {
    parsed <- suppressMessages(vroom::vroom(
      I(httr2::resp_body_string(res, encoding = "UTF-8")))
    )
  }
  if (type == "application/rds") {
    parsed <- unserialize(res$body)
    parsed <- change_grouped_stats_to_csv(parsed) # GC: used to pivot.
  }

  if (simplify) {
    httr2::resp_check_status(res, info = parsed$message)
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Formatting functions -------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PR 63
#' rename columns in dataframe
#'
#' @param df  data frame
#' @param oldnames  character: old names
#' @param newnames  character: new names
#'
#' @return data frame with new names
#' @keywords internal
rename_cols <- function(df, oldnames, newnames) {

  #   _______________________________________
  #   Defenses                               ####
  stopifnot( exprs = {
    is.data.frame(df)
    length(oldnames) == length(newnames)
    # all(oldnames %in% names(df))
  }
  )

  #   ___________________________________________
  #   Computations                              ####
  df_names <- names(df)

  old_position <- which(oldnames %in% df_names)
  old_available <- oldnames[old_position]
  new_available <- newnames[old_position]

  tochange <- vector(length = length(old_available))

  for (i in seq_along(old_available)) {
    tochange[i] <- which(df_names %in% old_available[i])
  }

  names(df)[tochange] <- new_available


  #   ____________________________________________
  #   Return                                      ####
  return(df)

}

#' Rename columns
#' TEMP function to rename response cols
#' @param df A data.frame
#' @param url response url
#' @noRd
tmp_rename_cols <- function(df, url = "") {
  # PR 63
  oldnames = c(
    "survey_year",
    "reporting_year",
    "reporting_pop",
    "reporting_gdp",
    "reporting_pce",
    "pce_data_level"
  )

  newnames = c("welfare_time",
               "year",
               "pop",
               "gdp",
               "hfce",
               "hfce_data_level")

  rename_cols(df,oldnames, newnames)
}


#' pip_is_transient
#'
#' Helper function to determine if an error is due to the number of requests
#' going over the rate limit
#'
#' @param resp A httr response
#'
#' @return logical
#'
pip_is_transient <- function(resp) {
  if (httr2::resp_is_error(resp)) {
    if (httr2::resp_status(resp) == 429) {
      stringr::str_detect(httr2::resp_body_json(resp, check_type = FALSE)$message,
                          "Rate limit is exceeded")
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

#' retry_after
#'
#' Helper function to determine how much time to wait before a new
#' query can be sent
#'
#' @param resp A httr response
#'
#' @return numeric
#'
retry_after <- function(resp) {
  if (httr2::resp_is_error(resp)) {
    time <- httr2::resp_body_json(resp, check_type = FALSE)$message
    time <- stringr::str_remove(time, "Rate limit is exceeded. Try again in ")
    readr::parse_number(time)
  } else {
    0
  }
}

#' parse_error_body
#'
#' Helper function to parse error messages generated by the PIP API
#'
#' @param resp A httr response
#'
#' @return character
#'
parse_error_body <- function(resp) {
  if (httr2::resp_is_error(resp)) {
    if (is_gateway_timeout(resp)) {
      # Handle gateway timeout
      return(httr2::resp_status_desc(resp))
    } else if (is_bad_gateway(resp)) {
      # Handle bad gateway timeout
      return(httr2::resp_status_desc(resp))
    }  else {
      # Handle regular PIP errors
      out <- httr2::resp_body_json(resp)
      message1 <- out$error[[1]]
      message2 <- out$details[[1]]$msg[[1]]
      message3 <- paste(unlist(out$details[[names(out$details)]]$valid), collapse = ", ")
      message <- c(message1, message2, message3)
      return(message)
    }
  }
}

is_gateway_timeout <- function(resp) {
  httr2::resp_status(resp) == 504 &
    httr2::resp_status_desc(resp) == "Gateway Timeout"
}

is_bad_gateway <- function(resp) {
  httr2::resp_status(resp) == 502 &
    httr2::resp_status_desc(resp) == "Bad Gateway"
}

#' Deletes content of the cache folder
#'
#'
#' @return Side effect. Deletes files.
#'
#' @export
#'
#' @examples \dontrun{delete_cache()}
delete_cache <- function() {

  cached_files <- list.files(tools::R_user_dir("pipr", which = "cache"),
                             full.names = TRUE)

  if (length(cached_files) == 0) {
    message("Cache is empty. Nothing to delete")
  } else {
    lapply(cached_files, file.remove)
    message("All items have been deleted from the cache.")
  }
}

#' Provides some information about cached items
#'
#'
#' @return character.
#'
#' @export
#'
#' @examples
#' \dontrun{get_cache_info()}
get_cache_info <- function() {

  cache_path <- tools::R_user_dir("pipr", which = "cache")
  n_cached <- length(list.files(cache_path))

  if (n_cached > 1) {
    message_text <-  " API responses are currently cached in "
  } else {
    message_text <-  " API response is currently cached in "
  }

    message(cli::format_message(c("Cache status:",
                                "i" = paste0(n_cached, message_text, cache_path))))
}


#' Change the list-output to dataframe (Function from pipapi)
#'
#' @param out output from wbpip::gd_compute_pip_stats
#'
#' @return dataframe
#' @export
change_grouped_stats_to_csv <- function(out) {
  out[paste0("decile", seq_along(out$deciles))] <- out$deciles
  out$deciles <- NULL
  data.frame(out)
}
