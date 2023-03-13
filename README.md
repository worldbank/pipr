
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipr

<!-- badges: start -->

[![R-CMD-check](https://github.com/worldbank/pipr/workflows/R-CMD-check/badge.svg)](https://github.com/worldbank/pipr/actions?workflow=R-CMD-check)
[![test-coverage](https://github.com/worldbank/pipr/workflows/test-coverage/badge.svg)](https://github.com/worldbank/pipr/actions)
[![pkgdown](https://github.com/worldbank/pipr/workflows/pkgdown/badge.svg)](https://github.com/worldbank/pipr/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/worldbank/pipr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/worldbank/pipr?branch=main)
<!-- badges: end -->

The `pipr` package allows R users to compute poverty and inequality
indicators for more than 160 countries and regions from the World Bank’s
database of household surveys. It does so by accessing the Poverty and
Inequality Platform (PIP) API. PIP is a computational tool that allows
users to estimate poverty rates for regions, sets of countries or
individual countries, over time and at any poverty line.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
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
#> Rows: 14
#> Columns: 40
#> $ region_name          <chr> "Europe & Central Asia", "Europe & Central Asia",~
#> $ region_code          <chr> "ECA", "ECA", "ECA", "ECA", "ECA", "ECA", "ECA", ~
#> $ country_name         <chr> "Albania", "Albania", "Albania", "Albania", "Alba~
#> $ country_code         <chr> "ALB", "ALB", "ALB", "ALB", "ALB", "ALB", "ALB", ~
#> $ year                 <dbl> 1996, 2002, 2005, 2008, 2012, 2014, 2015, 2016, 2~
#> $ reporting_level      <chr> "national", "national", "national", "national", "~
#> $ survey_acronym       <chr> "EWS", "LSMS", "LSMS", "LSMS", "LSMS", "HBS", "HB~
#> $ survey_coverage      <chr> "national", "national", "national", "national", "~
#> $ welfare_time         <dbl> 1996, 2002, 2005, 2008, 2012, 2014, 2015, 2016, 2~
#> $ welfare_type         <chr> "consumption", "consumption", "consumption", "con~
#> $ survey_comparability <dbl> 0, 1, 1, 1, 1, 2, 2, 2, 4, 2, 4, 3, 4, 3
#> $ comparable_spell     <chr> "1996", "2002 - 2012", "2002 - 2012", "2002 - 201~
#> $ poverty_line         <dbl> 2.15, 2.15, 2.15, 2.15, 2.15, 2.15, 2.15, 2.15, 2~
#> $ headcount            <dbl> 0.0053484604, 0.0109264739, 0.0059108568, 0.00199~
#> $ poverty_gap          <dbl> 9.821458e-04, 1.894350e-03, 8.972340e-04, 3.96411~
#> $ poverty_severity     <dbl> 3.047592e-04, 5.942301e-04, 2.285120e-04, 8.69032~
#> $ watts                <dbl> 1.203124e-03, 2.382840e-03, 1.042396e-03, 4.48972~
#> $ mean                 <dbl> 7.933157, 8.108228, 9.165974, 10.038168, 9.517231~
#> $ median               <dbl> 6.972102, 6.688141, 7.799790, 8.400199, 8.240384,~
#> $ mld                  <dbl> 0.1191043, 0.1648116, 0.1544128, 0.1488934, 0.138~
#> $ gini                 <dbl> 0.2701034, 0.3173898, 0.3059566, 0.2998467, 0.289~
#> $ polarization         <dbl> 0.2412933, 0.2689816, 0.2545287, 0.2473111, 0.249~
#> $ decile1              <dbl> 0.03863286, 0.03494002, 0.03482536, 0.03733625, 0~
#> $ decile2              <dbl> 0.05289347, 0.04859444, 0.04920109, 0.05136781, 0~
#> $ decile3              <dbl> 0.06378683, 0.05842059, 0.05977283, 0.06088472, 0~
#> $ decile4              <dbl> 0.07322042, 0.06738204, 0.06921183, 0.06983584, 0~
#> $ decile5              <dbl> 0.08379662, 0.07653102, 0.07988158, 0.07912079, 0~
#> $ decile6              <dbl> 0.09354903, 0.08839459, 0.09037069, 0.08924133, 0~
#> $ decile7              <dbl> 0.1082309, 0.1022859, 0.1037214, 0.1029873, 0.105~
#> $ decile8              <dbl> 0.1247387, 0.1198443, 0.1212641, 0.1192908, 0.122~
#> $ decile9              <dbl> 0.1489955, 0.1492508, 0.1483394, 0.1453520, 0.148~
#> $ decile10             <dbl> 0.2121557, 0.2543564, 0.2434117, 0.2445831, 0.229~
#> $ cpi                  <dbl> 0.3996353, 0.7016371, 0.7539579, 0.8201141, 0.917~
#> $ ppp                  <dbl> 50.35737, 50.35737, 50.35737, 50.35737, 50.35737,~
#> $ pop                  <dbl> 3168033, 3051010, 3011487, 2947314, 2900401, 2889~
#> $ gdp                  <dbl> 1633.552, 2247.497, 2675.508, 3298.478, 3736.339,~
#> $ hfce                 <dbl> 1714.813, 1685.368, 2079.244, 2819.736, 2989.866,~
#> $ is_interpolated      <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, ~
#> $ distribution_type    <chr> "micro", "micro", "micro", "micro", "micro", "mic~
#> $ estimation_type      <chr> "survey", "survey", "survey", "survey", "survey",~
```

### Access data dictionary

``` r
get_dictionary()
#> # A tibble: 41 x 2
#>    variable        definition                                                   
#>    <chr>           <chr>                                                        
#>  1 region_name     World Bank region name                                       
#>  2 region_code     Three-letter World Bank abbreviation of world regions        
#>  3 year            Year                                                         
#>  4 country_name    World Bank country name                                      
#>  5 country_code    Three-letter ISO (alpha-3) country code system for internati~
#>  6 reporting_level Reporting level                                              
#>  7 survey_acronym  Country survey acronym                                       
#>  8 survey_coverage Geographic coverage of the country survey (i.e. national, ur~
#>  9 welfare_time    Welfare time                                                 
#> 10 welfare_type    Type of welfare vector used for estimates (income or consump~
#> # ... with 31 more rows
```

## Citation

To cite package `pipr` in publications use:

      Tony Fujs, Aleksander Eilertsen, Ronak Shah and R. Andrés Castañeda (2022). pipr: Client for the PIP
      API. https://github.com/worldbank/pipr, https://worldbank.github.io/pipr/.

A BibTeX entry for LaTeX users is

      @Manual{,
        title = {pipr: Client for the PIP API},
        author = {Tony Fujs and Aleksander Eilertsen and Ronak Shah and R. Andrés Castañeda},
        year = {2022},
        note = {https://github.com/worldbank/pipr,https://worldbank.github.io/pipr/},
      }
