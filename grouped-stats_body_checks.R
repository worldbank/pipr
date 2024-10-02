# Grouped stats body checks

# Notes:
# 1. Error when cum_welfare != cum_population is not specific, it just says ! HTTP 404 Not Found. -> Should I block this?
# 2. Error when format = "arrow", when format rds and json I get a tibble with 10 rows, when format = csv I get 1 row (+ cols) (format is set to NULL in pipapi)
# 3. I don't think I can send other parameters to the endpoint, popshare doesn't work, nor any other parameter not declared in the endpoint.
# 4. Note that without mean and povline it fails (so there is not a default.)
# 5. I cannot supply the lorenz fit to the endpoint because in the endpoint it is passed with the other parameters to "as.numeric" and it makes it fail.

# 1. Body checks -----
## 1. Args ----
api_version = "v1"
format = "rds"
simplify = TRUE
server = NULL
endpoint = "grouped-stats"

cum_welfare =  c(0.0002,0.0006,0.0011,0.0021,0.0031,0.0048,0.0066,0.0095,0.0128,0.0177,0.0229,0.0355,0.0513,0.0689,0.0882)
cum_population = c(0.001,0.003,0.005,0.009,0.013,0.019,0.025,0.034,0.044,0.0581,0.0721,0.1041,0.1411,0.1792, 0.3)
requested_mean = 2
povline = 1.9
lorenz = "lq"
n_bins = NULL
# popshare = 0.5
#format = "csv"
#format = "json"
#format = "arrow"

## 2. Match args ----
#api_version <- match.arg(api_version)
#format <- match.arg(format)

## 3. Args checks ----
if (length(cum_welfare) != length(cum_population)) {
  cli::cli_abort("{.val cum_welfare} and {.val cum_population} must have the same length.")
}



## 4.1 Request grouped-stats ----
endpoint = "grouped-stats"
req_gd <- build_request_v2(
  cum_welfare     = cum_welfare,
  cum_population  = cum_population,
  requested_mean  = requested_mean,
  povline         = povline,
  format          = format,
  server          = server,
  api_version     = api_version,
  endpoint        = endpoint
  )

res <- req_gd |>
  httr2::req_perform()

parse_response(res, simplify = simplify)

## 4.2 Request lorenz-curve ----
endpoint = "lorenz-curve"
req_lorenz <- build_request_v2(
  cum_welfare     = cum_welfare,
  cum_population  = cum_population,
  #lorenz = lorenz,
  n_bins = n_bins,
  #format          = format,
  server          = server,
  api_version     = api_version,
  endpoint        = endpoint
)


res <- req_lorenz |>
  httr2::req_perform()

parse_response(res, simplify = simplify)


## 4.3 Request regression-params ----
endpoint = "regression-params"
req_lreg <- build_request_v2(
  cum_welfare     = cum_welfare,
  cum_population  = cum_population,
  #format          = format,
  server          = server,
  api_version     = api_version,
  endpoint        = endpoint
)


# 5. Perform request
res <- req_reg |>
  httr2::req_perform()

parse_response(res, simplify = simplify)



# 2. Function checks -----
cum_welfare =  c(0.0002,0.0006,0.0011,0.0021,0.0031,0.0048,0.0066,0.0095,0.0128,0.0177,0.0229,0.0355,0.0513,0.0689,0.0882)
cum_population = c(0.001,0.003,0.005,0.009,0.013,0.019,0.025,0.034,0.044,0.0581,0.0721,0.1041,0.1411,0.1792, 0.3)


## grouped_stats -----
get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  povline = 1.9,
                  requested_mean = 2.1,
                  endpoint = "grouped-stats")

### errors ----
get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  povline = NULL,
                  requested_mean = 2.1,
                  endpoint = "grouped-stats")

get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  povline = 1.9,
                  requested_mean = NULL,
                  endpoint = "grouped-stats")

cum_population = c(0.001,0.003,0.005,0.009,0.013,0.019,0.025,0.034,0.044,0.0581,0.0721,0.1041,0.1411,0.1792)
get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  povline = 1.9,
                  requested_mean = 2.1,
                  endpoint = "grouped-stats")


## lorenz-curve ----
get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  n_bins = 13,
                  endpoint = "lorenz-curve")




## regress-params -----
get_grouped_stats(cum_population = cum_population,
                  cum_welfare = cum_welfare,
                  endpoint = "regression-params")


