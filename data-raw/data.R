prod_url <- "https://api.worldbank.org/pip"
pipr_user_agent <- "pipr (https://worldbank.github.io/pipr/)"

usethis::use_data(
  prod_url,
  pipr_user_agent,
  internal = TRUE,
  overwrite = TRUE
)
