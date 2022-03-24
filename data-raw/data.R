## code to prepare `data` dataset goes here
prod_url <- "https://api.worldbank.org/pip"

usethis::use_data(prod_url,
                  internal = TRUE,
                  overwrite = TRUE)
