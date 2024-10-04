#' Read historical data
#'
#' @param years Years of data to be returned
#' @param sample_class The survey sample class, normally either 30 or 35
#'
#' @return a list
#' @export
sm_read_historical <- function(years, sample_class) {
  
  # Main data from mar dump ----------------------------------------------------
  ST <-
    mardata::stod |>
    dplyr::filter(ar %in% years) |>
    dplyr::left_join(mardata::syni,
                     by = "stod_id") |>
    dplyr::filter(synaflokkur_nr %in% sample_class) |>
    dplyr::mutate(index = dplyr::case_when(!is.na(reitur) & !is.na(tog_nr) & !is.na(veidarfaeri) ~ (reitur * 100 + tog_nr) * 100 + veidarfaeri),
                  .default = -1) |> 
    dplyr::select(synis_id,
                  leidangur,
                  dags,
                  skip = skip_nr,
                  stod = stod_nr,   # check
                  reitur,
                  smareitur,
                  kastad_n_breidd = kastad_breidd,
                  kastad_v_lengd = kastad_lengd,
                  hift_n_breidd = hift_breidd,
                  hift_v_lengd = hift_lengd,
                  dypi_kastad = botndypi_kastad,
                  dypi_hift = botndypi_hift,
                  veidarfaeri,
                  moskvastaerd,
                  grandaralengd,
                  #heildarafli,
                  synaflokkur = synaflokkur_nr,
                  ar,
                  togbyrjun,
                  togendir,
                  toghradi,
                  toglengd,
                  vir_uti,
                  lodrett_opnun,
                  tognumer = tog_nr,
                  togstefna,
                  larett_opnun,
                  togtimi = timi,
                  togdypi_kastad,
                  togdypi_hift,
                  togdypishiti,
                  # eykt,
                  vindhradi,
                  vindatt = vindatt_nr,
                  vedur = vedur_nr,
                  sky = sky_nr,
                  sjor = sjor_nr,
                  botnhiti,
                  yfirbordshiti,
                  hafis = hafis_nr,
                  straumstefna,
                  straumhradi,
                  sjondypi,
                  index
    ) |>
    dplyr::mutate(lon1 = kastad_v_lengd,
                  lat1 = kastad_n_breidd,
                  lon2 = hift_v_lengd,
                  lat2 = hift_n_breidd) |>
    dplyr::mutate(lon2 = ifelse(is.na(lon2), lon1, lon2),
                  lat2 = ifelse(is.na(lat2), lat1, lat2)) |> 
    dplyr::mutate(lon = (lon1 + lon2) / 2,
                  lat = (lat1 + lat2) / 2,
                  toglengd = ifelse(is.na(toglengd), 4, toglengd))
  
  NU <-
    ST |> 
    dplyr::select(synis_id, ar, index) |> 
    dplyr::left_join(mardata::skala |>
                       # strange to have na in tegund_nr
                       dplyr::filter(!is.na(tegund_nr)),
                     by = dplyr::join_by(synis_id)) |>
    dplyr::select(synis_id,
                  ar, 
                  index,
                  tegund = tegund_nr,
                  fj_maelt = maeldir,
                  fj_talid = taldir) |>
    dplyr::mutate(fj_alls = fj_maelt + fj_talid,
                  r = dplyr::case_when(fj_maelt == 0 ~ 1,
                                       .default = fj_alls / fj_maelt))
  LE <-
    ST |> 
    dplyr::select(synis_id, ar, index) |> 
    dplyr::left_join(mardata::lengd,
                     by = dplyr::join_by(synis_id)) |> 
    dplyr::group_by(synis_id, ar, index, tegund = tegund_nr, lengd) |> 
    dplyr::summarise(fjoldi = sum(fjoldi, na.rm = TRUE),
                     .groups = "drop") |> 
    dplyr::left_join(NU |> 
                       dplyr::select(synis_id, ar, index, tegund, r),
                     by = dplyr::join_by(synis_id, ar, index, tegund)) |> 
    dplyr::mutate(n = fjoldi * r,
                  b = (n * 0.01 * lengd^3) / 1e3) |> 
    # some odd measure
    dplyr::filter(!(ar == 1993 & index == 6660273 & tegund == 41 & is.na(r))) |> 
    dplyr::filter(!(ar == 1986 & index == 5630273 & tegund == 19 & is.na(lengd)))
  
  # NOTE: This happens e.g. if one gets a zero station
  # if(any(is.na(LE$r))) stop("Unexpected: Raising factor (r) in object LE is na")
  # if(any(is.na(LE$lengd))) stop("Unexpected: Undefined lengd in object LE")
  
  KV <-
    ST |> 
    dplyr::select(synis_id, ar, index) |> 
    dplyr::inner_join(mardata::aldur,
                      by = dplyr::join_by(synis_id)) |> 
    dplyr::select(synis_id,
                  ar,
                  index,
                  tegund = tegund_nr,
                  nr = kvarna_nr,
                  lengd,
                  kyn = kyn_nr,
                  kynthroski_nr,
                  aldur,
                  oslaegt = thyngd,
                  slaegt,
                  kynfaeri,
                  lifur,
                  magi)
  
  return(list(ST = ST, NU = NU, LE = LE, KV = KV))
  
}