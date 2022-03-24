.onAttach  <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    get_stats <<- memoise::memoise(get_stats, cache = cm)
    get_aux <<- memoise::memoise(get_aux, cache = cm)
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
