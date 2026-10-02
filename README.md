
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipr

<!-- badges: start -->

[![CRAN
version](https://img.shields.io/cran/v/pipr)](https://CRAN.R-project.org/package=pipr)
[![R-CMD-check](https://img.shields.io/github/actions/workflow/status/worldbank/pipr/R-CMD-check.yaml?branch=main&label=R-CMD-check)](https://github.com/worldbank/pipr/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://img.shields.io/github/actions/workflow/status/worldbank/pipr/test-coverage.yaml?branch=main&label=test-coverage)](https://github.com/worldbank/pipr/actions/workflows/test-coverage.yaml)
[![pkgdown](https://img.shields.io/github/actions/workflow/status/worldbank/pipr/pkgdown.yaml?branch=main&label=pkgdown)](https://github.com/worldbank/pipr/actions/workflows/pkgdown.yaml)
[![Codecov test
coverage](https://codecov.io/gh/worldbank/pipr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/worldbank/pipr?branch=main)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The `pipr` package allows R users to compute poverty and inequality
indicators for more than 160 countries and regions from the World Bank’s
database of household surveys. It does so by accessing the Poverty and
Inequality Platform (PIP) API. PIP is a computational tool that allows
users to estimate poverty rates for regions, sets of countries or
individual countries, over time and at any poverty line.

## Installation

`pipr` 1.5.0 is available on
[CRAN](https://CRAN.R-project.org/package=pipr). Install the released
version with:

``` r
install.packages("pipr")
```

You can also install the development version from
[GitHub](https://github.com/worldbank/pipr):

``` r
devtools::install_github("worldbank/pipr")
```

## Example

This is a basic example that shows how to retrieve some key poverty and
inequity statistics.

### Retrieve statistics

``` r
library(dplyr)
library(pipr)

df <- get_stats(country = "ALB")
glimpse(df)
```

### Access data dictionary

``` r
get_aux("dictionary")
```
