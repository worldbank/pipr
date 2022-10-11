
.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    get_stats <<- memoise::memoise(get_stats, cache = cm)
    get_wb <<- memoise::memoise(get_wb, cache = cm)
    get_aux <<- memoise::memoise(get_aux, cache = cm)
  }

  options(cli.ignore_unknown_rstudio_theme = TRUE)

  if (!".pip" %in% ls(envir = .GlobalEnv, all.names = TRUE)) {

    .pip <<- new.env()

  } else {
    pip_ls <- ls(envir = .pip)
    if (length(pip_ls) > 0) {
      cli::cli({
        cli::cli_inform("Environment {.field .pip} already exists.
                      It contains the following obejcts:")
        cli::cli_ul(pip_ls)
      })

      cli::cli_inform("To restart the {.field .pip} env, type {.code base::rm(.pip)}")
    }
  }

}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
