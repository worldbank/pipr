
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipr

<!-- badges: start -->

[![R-CMD-check](https://github.com/PIP-Technical-Team/pipr/workflows/R-CMD-check/badge.svg)](https://github.com/PIP-Technical-Team/pipr/actions?workflow=R-CMD-check)
[![test-coverage](https://github.com/PIP-Technical-Team/pipr/workflows/test-coverage/badge.svg)](https://github.com/PIP-Technical-Team/pipr/actions)
[![pkgdown](https://github.com/PIP-Technical-Team/pipr/workflows/pkgdown/badge.svg)](https://github.com/PIP-Technical-Team/pipr/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The `pipr` package allows R users to compute poverty and inequality
indicators for more than 160 countries and regions from the World Bank’s
database of household surveys.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipr")
```

## Example

This is a basic example that shows how to retrieve some key poverty and
inequity statistics.

``` r
library(dplyr)
library(pipr)

df <- get_stats(country = "ALB")
glimpse(df)
#> Rows: 9
#> Columns: 47
#> $ region_code             <chr> "ECA", "ECA", "ECA", "ECA", "ECA", "ECA", "ECA~
#> $ country_code            <chr> "ALB", "ALB", "ALB", "ALB", "ALB", "ALB", "ALB~
#> $ reporting_year          <dbl> 1996, 2002, 2005, 2008, 2012, 2014, 2015, 2016~
#> $ reporting_level         <chr> "national", "national", "national", "national"~
#> $ survey_acronym          <chr> "EWS", "LSMS", "LSMS", "LSMS", "LSMS", "HBS", ~
#> $ survey_coverage         <chr> "national", "national", "national", "national"~
#> $ survey_year             <dbl> 1996, 2002, 2005, 2008, 2012, 2014, 2015, 2016~
#> $ welfare_type            <chr> "consumption", "consumption", "consumption", "~
#> $ survey_comparability    <dbl> 0, 1, 1, 1, 1, 2, 2, 2, 2
#> $ comparable_spell        <chr> "1996", "2002 - 2012", "2002 - 2012", "2002 - ~
#> $ poverty_line            <dbl> 1.9, 1.9, 1.9, 1.9, 1.9, 1.9, 1.9, 1.9, 1.9
#> $ headcount               <dbl> 0.009206690, 0.015708434, 0.008605271, 0.00313~
#> $ poverty_gap             <dbl> 0.0013985640, 0.0026143517, 0.0013114397, 0.00~
#> $ poverty_severity        <dbl> 0.0004106306, 0.0007931669, 0.0003343455, 0.00~
#> $ watts                   <dbl> 0.001695347, 0.003246725, 0.001527466, 0.00059~
#> $ mean                    <dbl> 6.572457, 6.717500, 7.593820, 8.311160, 7.8828~
#> $ median                  <dbl> 5.776242, 5.540987, 6.461965, 6.954994, 6.8252~
#> $ mld                     <dbl> 0.1191043, 0.1648116, 0.1544128, 0.1488934, 0.~
#> $ gini                    <dbl> 0.2701034, 0.3173898, 0.3059566, 0.2998467, 0.~
#> $ polarization            <dbl> 0.2412933, 0.2689816, 0.2545287, 0.2473111, 0.~
#> $ decile1                 <dbl> 0.03863286, 0.03494002, 0.03482536, 0.03733625~
#> $ decile2                 <dbl> 0.05289347, 0.04859444, 0.04920109, 0.05136781~
#> $ decile3                 <dbl> 0.06378683, 0.05842059, 0.05977283, 0.06088472~
#> $ decile4                 <dbl> 0.07322042, 0.06738204, 0.06921183, 0.06983584~
#> $ decile5                 <dbl> 0.08379662, 0.07653102, 0.07988158, 0.07912079~
#> $ decile6                 <dbl> 0.09354903, 0.08839459, 0.09037069, 0.08924133~
#> $ decile7                 <dbl> 0.1082309, 0.1022859, 0.1037214, 0.1029873, 0.~
#> $ decile8                 <dbl> 0.1247387, 0.1198443, 0.1212641, 0.1192908, 0.~
#> $ decile9                 <dbl> 0.1489955, 0.1492508, 0.1483394, 0.1453520, 0.~
#> $ decile10                <dbl> 0.2121557, 0.2543564, 0.2434117, 0.2445831, 0.~
#> $ survey_mean_lcu         <dbl> 159.6515, 286.4848, 348.0076, 414.5642, 439.57~
#> $ survey_mean_ppp         <dbl> 6.572457, 6.717500, 7.593820, 8.311160, 7.8828~
#> $ predicted_mean_ppp      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ cpi                     <dbl> 0.4444618, 0.7803388, 0.8385284, 0.9126820, 1.~
#> $ cpi_data_level          <chr> "national", "national", "national", "national"~
#> $ ppp                     <dbl> 54.65258, 54.65258, 54.65258, 54.65258, 54.652~
#> $ ppp_data_level          <chr> "national", "national", "national", "national"~
#> $ reporting_pop           <dbl> 3168033, 3051010, 3011487, 2947314, 2900401, 2~
#> $ pop_data_level          <chr> "national", "national", "national", "national"~
#> $ reporting_gdp           <dbl> 1633.642, 2247.705, 2675.761, 3298.606, 3736.3~
#> $ gdp_data_level          <chr> "national", "national", "national", "national"~
#> $ reporting_pce           <dbl> 1714.813, 1685.368, 2079.244, 2819.736, 2989.8~
#> $ pce_data_level          <chr> "national", "national", "national", "national"~
#> $ is_interpolated         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~
#> $ is_used_for_aggregation <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS~
#> $ distribution_type       <chr> "micro", "micro", "micro", "micro", "micro", "~
#> $ estimation_type         <chr> "survey", "survey", "survey", "survey", "surve~
```
