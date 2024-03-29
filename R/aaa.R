.pip <-  new.env(parent = emptyenv())


#' Set auxiliary table in .pip environment for later call
#'
#' @param table character: name of the table in .pip env
#' @param value data to be saved
#' @inheritParams get_aux
#'
#' @return Invisible TRUE if set correctly. FALSE otherwise
#' @keywords internal
set_aux <- function(table,
                    value,
                    force = FALSE) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Evaluate if exists --------

  to_set <-  1
  if (rlang::env_has(.pip, table)) {
    if (force == FALSE) {
      cli::cli_alert("Table {.field {table}} already exists.")
      to_set <- utils::menu(c("Replace with new table", "Abort"))
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defense --------

  if (to_set == 2) {
    msg     <- c("Setting {.field {table}} into {.code .pip} aborted")
    cli::cli_abort(msg, wrap = TRUE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## set table into .pip --------

  rlang::env_poke(env = .pip,
                  nm = table,
                  value = value)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  was_set <- rlang::env_has(.pip, table)

  if (isFALSE(was_set)) {
    msg     <- c("Table {.field {table}} could not be set into {.code .pip}")
    cli::cli_alert(msg,
                   wrap = TRUE
    )
  }
  return(invisible(was_set))

}


#' call a table from .pip env
#'
#' @param table character: name of table in .pip env. If NULL, it displays the
#'   names of tables available in .pip env
#'
#' @return data frame of auxiliary table
#' @export
#'
#' @examples
#' # call one table
#'
#' get_aux("gdp", assign_tb = TRUE, force = TRUE)
#' call_aux("gdp")
#'
#' # see the name of several tables in memory
#' tb <- c("cpi", "ppp", "pop")
#' lapply(tb, get_aux, assign_tb = TRUE, force = TRUE)
#' call_aux()
call_aux <- function(table = NULL) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot({
    length(table) == 1 || is.null(table)
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # call aux   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## If NULL --------

  if (is.null(table)) {
    nms <- rlang::env_names(.pip)

    if (length(nms) == 0) {
      cli::cli_alert_info("no tables available in {.env .pip} environment")
      return(invisible(nms))
    }

    run_cli     <- run_cli()
    txt_to_ex <- paste0("{.",
                        ifelse(run_cli, "run", "code"),
                        " [{.x}]({torun})}")

    cli::cli_h2("tables available in env {.env pip}")
    purrr::walk(.x = nms,
                .f = ~{
                  torun <- paste0("pipr::call_aux(table = ", shQuote(.x), ")")

                  cli::cli_text(txt_to_ex)
                })

    return(invisible(nms))

  } else {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## If table is selected --------
    if (rlang::env_has(.pip, table)) {
      return(rlang::env_get(.pip, table))
    } else {
      msg     <- c("*" = "Table {.field {table}} does not exist")
      cli::cli_abort(msg, wrap = TRUE)
    }

  }

}



#' whether or not to run cli or just to show code
#'
#' @return logical
#' @keywords internal
run_cli <- function() {
  cli_types   <-
    cli::ansi_hyperlink_types() |>
    names()

  run_cli <- "run" %in% cli_types
  return(invisible(run_cli))
}
