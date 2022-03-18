% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stats.R
\name{get_stats}
\alias{get_stats}
\title{Get poverty and inequality statistics}
\usage{
get_stats(
  country = "all",
  year = "all",
  povline = 1.9,
  popshare = NULL,
  fill_gaps = FALSE,
  group_by = NULL,
  welfare_type = c("all", "income", "consumption"),
  reporting_level = c("all", "national", "urban", "rural"),
  version = NULL,
  api_version = "v1",
  format = c("json", "csv", "rds"),
  simplify = TRUE,
  server = NULL
)
}
\arguments{
\item{country}{character: A vector with one or more country ISO 3 codes or
'all'}

\item{year}{integer: A vector with one or more years or 'all'}

\item{povline}{numeric: Poverty line}

\item{popshare}{numeric: Proportion of the population living below the
poverty line}

\item{fill_gaps}{logical: If TRUE, will interpolate / extrapolate values for
missing years}

\item{group_by}{character: If used result will be aggregated for predefined
sub-groups}

\item{welfare_type}{character: Welfare type}

\item{reporting_level}{character: Geographical reporting level}

\item{version}{character: Data version. See \code{get_versions()}}

\item{api_version}{character: API version}

\item{format}{character: Response format}

\item{simplify}{logical: If TRUE (default) the response is converted to a
data frame}

\item{server}{character: Server. For WB internal use only}
}
\value{
tibble or list
}
\description{
Get poverty and inequality statistics
}
\examples{
\dontrun{
# One country-year
res <- get_stats(country = "AGO", year = 2000)

# All years for a specific country
res <- get_stats(country = "AGO", year = "all")

# All countries and years
res <- get_stats(country = "all", year = "all")

# All countries and years w/ alternative poverty line
res <- get_stats(country = "all", year = "all", povline = 3.2)

# Fill gaps for years without available survey data
res <- get_stats(country = "all", year = "all", fill_gaps = TRUE)

# Proportion living below the poverty line
res <- get_stats(country = "all", year = "all", popshare = .4)

# World Bank global and regional aggregates
get_stats("all", year = "all", group_by = "wb")

# Custom aggregates
get_stats(c("ARG", "BRA"), year = 2018, group_by = "none")

}
}