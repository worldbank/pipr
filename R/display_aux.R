#' Display available auxiliary tables
#'
#' @inheritParams get_stats
#' @inheritParams get_aux
#'
#' @return invisible tibble with names of auxiliary tables
#' @export
#'
#' @examples
#' \dontrun{
#' display_aux()
#' }
display_aux <- function(version         = NULL,
                        ppp_version     = NULL,
                        release_version = NULL,
                        api_version     = "v1",
                        format          = c("rds", "json", "csv"),
                        simplify        = TRUE,
                        server          = NULL,
                        assign_tb       = FALSE) {

  # Match args
  api_version <- match.arg(api_version)
  format      <- match.arg(format)
  il          <- as.list(environment())
  fun_args    <- args_to_string(il)
  cli_types   <-
    cli::ansi_hyperlink_types() |>
    names()
  run_cli <- "run" %in% cli_types


#   ____________________________________________________________________________
#   Build query string                                                  ####

  u      <- build_url(server, "aux", api_version = api_version)
  res    <- httr::GET(u)
  tbs_tb <- parse_response(res, simplify = simplify)
  tbs    <- tbs_tb[["tables"]]
  if (isTRUE(run_cli)) {

    cli::cli_h2("Click on any of the tables below")
    purrr::walk(.x = tbs,
                .f = ~{
                  torun <- paste0("pipr::get_aux(table = ", shQuote(.x), ",",
                                  fun_args,")")
                  y <- gsub("_", " ", .x)
                  cli::cli_text(
                    "{.run [{y}]({torun})}"
                  )
                })
  } else {
    cat("Available Auxiliary tables:")
    tbs_tb
  }


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(invisible(tbs_tb))

}

#' convert arguments and values of a function to a string to parse into other
#' functions
#'
#' @param il list from `as.list(environment())` right after the function is
#'   called.
#'
#' @return character
args_to_string <- function(il) {


#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {
    is.list(il)
    !is.null(names(il))
    }
  )



#   ____________________________________________________________________________
#   Computations                                                            ####
    nil <- names(il)

    tl <- sapply(il, typeof) # type of each arguments

    # just character args
    ch <- tl == "character"
    chl <- toString(paste(nil[ch], " = ", shQuote(il[ch])))

    # No character args
    no_chl <- paste(nil[!ch], " = ", il[!ch], collapse = ", ")

    #combine
    pl <- paste(chl, no_chl, sep = ", ")


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(pl)

}