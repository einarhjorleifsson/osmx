#' Standardize by towlength
#'
#' @param res The hafvog list
#' @param tow A character vector specifying min, standard and max towlength
#'
#' @return a list
#' @export
#'
sm_standardize_by_tow <- function(res, tow = c(2, 4, 8)) {
  
  res$LE <- 
    res$LE |> 
    dplyr::left_join(res$ST |> 
                       dplyr::select(ar, index, toglengd),
                     by = dplyr::join_by(ar, index)) |> 
    dplyr::mutate(toglengd = 
                    dplyr::case_when(is.na(toglengd) ~ tow[2],       # should really throw an error
                                     toglengd > max(tow) ~ max(tow),
                                     toglengd < min(tow) ~ min(tow),
                                     .default = toglengd),
                  n = n / toglengd * tow[2],    # standardize to 4 miles
                  b = b / toglengd * tow[2])
  
  return(res)
  
}