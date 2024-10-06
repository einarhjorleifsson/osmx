#' Standardize by towlength
#'
#' @param res The hafvog list
#' @param tow A character vector specifying min, standard and max towlength
#'
#' @return a list
#' @export
#'
sm_standardize_by_tow <- function(lengdir, tow = c(2, 4, 8)) {

  lengdir <- 
    lengdir |> 
    dplyr::mutate(toglengd = 
                    dplyr::case_when(is.na(toglengd) ~ tow[2],       # should really throw an error
                                     toglengd > max(tow) ~ max(tow),
                                     toglengd < min(tow) ~ min(tow),
                                     .default = toglengd),
                  n = n / toglengd * tow[2],    # standardize to 4 miles
                  b = b / toglengd * tow[2])
  
  return(lengdir)
  
}



#' Calculate coordinates
#'
#' @param res A list
#'
#' @return A list
#' @export
#'
sm_calc_coords <- function(res) {
  
  res$ST <-
    res$ST |> 
    dplyr::mutate(lon1 = -kastad_v_lengd,
                  lat1 =  kastad_n_breidd,
                  lon2 = -hift_v_lengd,
                  lat2 =  hift_n_breidd,
                  lon = dplyr::case_when(is.na(lon2) ~ lon1,
                                         !is.na(lon1) & !is.na(lon2) ~ (lon1 + lon2) / 2,
                                         .default = lon1),
                  lat = dplyr::case_when(is.na(lat2) ~ lat1,
                                         !is.na(lat1) & !is.na(lat2) ~ (lat1 + lat2) / 2,
                                         .default = lat1))  
  
  return(res)
  
}

#' Add coordinates to length data
#'
#' @param res A list
#'
#' @return A list
#' @export
#'
sm_add_coords_to_length <- function(res) {
  
  res$LE <- 
    res$LE |> 
    dplyr::left_join(res$ST |>
                       dplyr::select(ar, index, lon, lat),
                     by = dplyr::join_by(ar, index))
  
  return(res)

}