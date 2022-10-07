#' Display available auxiliary tables
#'
#' @inheritParams get_stats
#' @inheritParams get_aux
#'
#' @return
#' @export
#'
#' @examples
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

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  # Build query string
  u      <- build_url(server, "aux", api_version = api_version)
  res    <- httr::GET(u)
  tbs_tb <- parse_response(res, simplify = simplify)
  tbs    <- tbs_tb[["tables"]]

  purrr::walk(.x = tbs,
              .f = ~{
                torun <- paste0('pipr::get_aux(table ="', .x,'")')
                cli::cli_text(
                  "Get {.run [{.x}]({torun})} table"
                )
              })

#   ____________________________________________________________________________
#   Return                                                                  ####
  return(invisible(tbs_tb))

}
