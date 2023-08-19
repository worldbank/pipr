pipr_default_options <- list(
  pipr.version  = NULL,
  pipr.ppp_version  = NULL,
  pipr.release_version = NULL,
  pipr.table  = NULL
)

.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    # d <- rappdirs::user_cache_dir("pipr")
    # cm <- cachem::cache_disk(d,
    #                          evict = "lru",
    #                          max_size = 512 * 1024^2)
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    get_stats <<- memoise::memoise(get_stats, cache = cm)
    get_wb <<- memoise::memoise(get_wb, cache = cm)
    get_aux <<- memoise::memoise(get_aux, cache = cm)
    get_versions <<- memoise::memoise(get_versions, cache = cm)
  }
  op    <- options()
  toset <- !(names(pipr_default_options) %in% names(op))
  if (any(toset)) options(pipr_default_options[toset])

  options(cli.ignore_unknown_rstudio_theme = TRUE)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
