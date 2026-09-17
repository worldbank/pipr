# pipr

<!-- cg:auto:overview -->
pipr is an R package that provides an interface to compute poverty and inequality indicators for more than 160 countries and regions from the World Bank's database of household surveys, through the Poverty and Inequality Portal (PIP).

Maintained by the World Bank (DECDG / GPID). Requires R >= 4.1.0. Licensed under MIT.
<!-- cg:auto:end -->

## Contents
- [API Reference](api-reference.md)
- [Vignettes](vignettes.md)
- [Changelog](changelog.md)

<!-- cg:auto:installation -->
Install from CRAN or GitHub:

```r
# CRAN
install.packages("pipr")

# GitHub development version
devtools::install_github("worldbank/pipr")
```
<!-- cg:auto:end -->

<!-- cg:auto:quick-start -->
```r
library(pipr)

# Compute poverty and inequality statistics
get_stats(country = "all", povline = 2.15, year = 2019)
```
<!-- cg:auto:end -->