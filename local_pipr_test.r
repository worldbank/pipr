# 0. Set-up (no-store branches) ----
## load no-store branch of pipr
library(devtools)
load_all()

## install no-store branch of pipapi
# devtools::install_github("PIP-Technical-Team/pipapi@no-store")

# 1. Initialize the api through pipapi ----
## 1.1 Go to no-store branch of pipapi, inst/TMP folder
## 1.2 Open TMP_local_steup.R and follow instructions.

# 2. get_stats() (pip) test ----
req <- httr2::request("http://127.0.0.1:8080/api/v1/pip") |>
  httr2::req_url_query(country = "COL", year = "2008",
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

## Perform request
res <- req |>
  httr2::req_perform()

## Parse result
out <- parse_response(res, simplify = TRUE)


# 3. get_gd() (grouped-stats) test ----
cum_welfare <- paste(datt_rural$L, collapse = ",")
cum_population <- paste(datt_rural$p, collapse = ",")

requested_mean <- 109
povline <- 89

# Build the query string
query_string <- paste0("http://127.0.0.1:8080",
  "/api/v1/grouped-stats?",
  "cum_welfare=", cum_welfare, "&",
  "cum_population=", cum_population, "&",
  "requested_mean=", requested_mean, "&",
  "povline=", povline
)

req <- httr2::request(query_string) |>
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

## Parse result
out <- parse_response(res, simplify = TRUE)

# 4. get_cp() test ----
country = "ITA"
povline = 2.15 # GC: default value like Stata
ppp_version = 2017 # GC: default value like Stata

query_string <- paste0("http://127.0.0.1:8080",
                       "/api/v1/cp-download?",
                       "country=", country, "&",
                       "povline=", povline, "&",
                       "ppp_version=", ppp_version)

req <- httr2::request(query_string) |>
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





