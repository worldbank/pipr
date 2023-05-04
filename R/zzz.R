
.onLoad <- function(libname, pkgname) {

  options(cli.ignore_unknown_rstudio_theme = TRUE)

}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    # packageStartupMessage("Info: Session based caching is enabled.")
  }
}
