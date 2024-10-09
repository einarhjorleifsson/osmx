sm_calc_capture_probability <- function(history) {
  
  dx <- 1
  h1 <- 
    history$stodvar |> 
    dplyr::mutate(ar = lubridate::year(dags),
                  lon = dplyr::case_when(is.na(hift_lengd) ~ kastad_lengd,
                                         !is.na(kastad_lengd) & !is.na(hift_lengd) ~ (kastad_lengd + hift_lengd) / 2,
                                         .default = kastad_lengd),
                  lat = dplyr::case_when(is.na(hift_breidd) ~ kastad_breidd,
                                         !is.na(kastad_breidd) & !is.na(hift_breidd) ~ (kastad_breidd + hift_breidd) / 2,
                                         .default = kastad_breidd)) |> 
    dplyr::select(leidangur, synis_id, ar, lon, lat) |> 
    tidyr::drop_na() |> 
    dplyr::mutate(lon = grade(lon, dx),
                  lat = grade(lat, dx/2)) 
  st_per_square <- 
    h1 |> 
    dplyr::count(lon, lat, name = "total")
  h2 <- 
    h1 |>  
    dplyr::left_join(history$numer |> 
                       dplyr::select(leidangur, synis_id, tegund) |> 
                       dplyr::mutate(present = TRUE),
                     by = dplyr::join_by(leidangur, synis_id)) |> 
    dplyr::select(lon, lat, tegund, present) |> 
    dplyr::count(lon, lat, tegund)
  p_per_square <- 
    st_per_square |> 
    dplyr::left_join(h2,
                     by = dplyr::join_by(lon, lat)) |> 
    tidyr::complete(tegund, tidyr::nesting(lon, lat, total),
                    fill = list(n = 0)) |> 
    dplyr::mutate(p = n / total)
  
  return(p_per_square)
  
}

sm_calc_by_square <- function(res, dx = 1) {
  res$by.station |> 
    dplyr::mutate(lon = grade(lon, dx),
                  lat = grade(lat, dx/2)) |> 
    dplyr::group_by(ar, lon, lat, tegund, var) |> 
    dplyr::reframe(val = sum(val))
}
