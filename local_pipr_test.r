library(devtools)
load_all()

# Version 1
server = "http://127.0.0.1:8080"
api_version = "v1"
endpoint = "pip"

req <- httr2::request(server) |>
  httr2::req_url_path_append(api_version) |>
  httr2::req_url_path_append(endpoint) |>
  httr2::req_url_query(country = "COL", year = "2008", povline = 2.15, fill_gaps = FALSE, additional_ind = FALSE, ppp_version = 2017) |>
  httr2::req_cache(tools::R_user_dir("pipr", which = "cache"),
                   use_on_error = TRUE,
                   debug = TRUE) |>
  httr2::req_verbose()

# Version 2
req <- httr2::request("http://127.0.0.1:8080/api/v1/pip?country=COL&year=all&povline=2.15&fill_gaps=false&additional_ind=false&ppp_version=2017")

req <- httr2::request("http://127.0.0.1:8080/api/v1/pip") |>
  httr2::req_url_query(country = "COL", year = "all",
                       povline = 2.15, fill_gaps = "false",
                       additional_ind = "false", ppp_version = 2017) |>
  httr2::req_cache(tools::R_user_dir("pipr", which = "cache"),
                   use_on_error = TRUE,
                   debug = TRUE) |>
  httr2::req_user_agent(pipr_user_agent) |>
  httr2::req_error(body = parse_error_body) |>
  httr2::req_retry(
    is_transient = pip_is_transient,
    after = retry_after,
    max_seconds = 60
  ) |>
  httr2::req_verbose()

# Perform request
res <- req |>
  httr2::req_perform()

# Parse result
out <- parse_response(res, simplify)



