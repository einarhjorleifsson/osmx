
#' Create a table where measurements are susspect
#'
#' @param res A list
#'
#' @return a DT table
#' @export
#'
sm_table_kvarnir <- function(res) {
  d <- 
    res$kv.this.year |> 
    dplyr::filter(.l_osl == "check" | .l_sl == "check" | .sl_osl == "check" | .kyn == "check" | .lif == "check") |> 
    dplyr::select(lestnr, tegund, nr, lengd, oslaegt, slaegt, kynfaeri, lifur, .l_osl, .l_sl, .sl_osl, .kyn, .lif)
  table <- 
    d |> 
    DT::datatable(extensions = 'Scroller',
                  rownames = FALSE,
                  options = list(deferRender = TRUE,
                                 scrollY = 700,
                                 scroller = TRUE
                  )) |> 
    DT::formatStyle('.l_osl', backgroundColor = DT::styleEqual(c("check", "ok", "na"), c('pink', '#C1FAAD', "grey"))) |> 
    DT::formatStyle('.l_sl', backgroundColor = DT::styleEqual(c("check", "ok", "na"), c('pink', '#C1FAAD', "grey"))) |>
    DT::formatStyle('.sl_osl', backgroundColor = DT::styleEqual(c("check", "ok", "na"), c('pink', '#C1FAAD', "grey"))) |>
    DT::formatStyle('.kyn', backgroundColor = DT::styleEqual(c("check", "ok", "na"), c('pink', '#C1FAAD', "grey"))) |> 
    DT::formatStyle('.lif', backgroundColor = DT::styleEqual(c("check", "ok", "na"), c('pink', '#C1FAAD', "grey")))
  
  return(table) 
}

#' A list of prey measurments
#'
#' @param d A tibble
#'
#' @return a DT table
#' @export
#'
sm_table_prey <- function(d) {
  d |> 
    DT::datatable(extensions = 'Scroller',
                  rownames = FALSE,
                  options = list(deferRender = TRUE,
                                 scrollY = 700,
                                 scroller = TRUE
                  ))
}

