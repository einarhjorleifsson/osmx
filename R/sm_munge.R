# remotes::install_github("einarhjorleifsson/ovog")
# devtools::load_all()
# maelingar <- c("data-raw/SMH/TB2-2024.zip", "data-raw/SMH/TTH1-2024.zip")
# stillingar <- c("data-raw/SMH/stillingar_SMH_rall_(haust).zip")
# tmp <- sm_munge(maelingar, stillingar)

#' Prepares data for the smxapp
#'
#' @param maelingar Names of hafvog zip files to be imported
#' @param stillingar Name of "stillingar" zip file to be imported
#' @param current.year The current survey year
#'
#' @return a list
#' @export
#'
sm_munge <- function(maelingar, stillingar, current.year = lubridate::year(lubridate::today())) {
  
  # IMPORT --------------------------------------------------------------------
  coloured_print("IMPORT", colour = "green")
  coloured_print("Import current measurements", colour = "green")
  if(!any(file.exists(maelingar))) {
    warning("At least one of the measurments files does not exist")
    tibble::tibble(file = maelingar,
                   exists = file.exists(maelingar)) |> 
      knitr::kable(caption = "List of files to import and if they exist or not")
    stop("Fix the file path of measurement files")
  }
  res <-
    ovog::hv_import_cruise(maelingar, collapse_station = TRUE) |> 
    # NOTE: no longer raised by counted
    ovog::hv_create_tables()
  
  res$stodvar <- 
    res$stodvar |> 
    dplyr::rename(kastad_lengd = kastad_v_lengd,
                  kastad_breidd = kastad_n_breidd,
                  hift_lengd = hift_v_lengd,
                  hift_breidd = hift_n_breidd,
                  veidarfaeri = fishing_gear_no) |> 
    dplyr::mutate(kastad_lengd = -kastad_lengd,
                  hift_lengd   = -hift_lengd,
                  index = dplyr::case_when(!is.na(reitur) & !is.na(tognumer) & !is.na(veidarfaeri) ~ (reitur * 100 + tognumer) * 100 + veidarfaeri,
                                           .default = -1))
  if(any(is.na(res$stodvar$reitur))) coloured_print("Reitur is missing, this may create trouble downstream", colour = "red")
  if(any(is.na(res$stodvar$tognumer))) coloured_print("Tognumer is missing, this may create trouble downstream", colour = "red")
  if(any(is.na(res$stodvar$veidarfaeri))) coloured_print("Veidarfaeri missing, this may create trouble downstream", colour = "red")
  
  current.synaflokkur <- unique(res$stodvar$synaflokkur)
  coloured_print(paste0("The 'synaflokkur' of the zip files is: ", current.synaflokkur), colour = "green") 
  if(length(current.synaflokkur) > 1) {
    stop(paste("There are more than one synaflokkur in the measurement files (",
               current.synaflokkur,
               ")"))
  }
  if(is.null(current.synaflokkur)) stop("Synaflokkur is NULL")
  
  ## Stillingar -----------------------------------------------------------------
  coloured_print("Import setup ('stillingar')", colour = "green")
  if(!any(file.exists(stillingar))) {
    warning("At least one of the setup ('stillingar') files does not exist")
    tibble::tibble(file = stillingar,
                   exists = file.exists(stillingar)) |> 
      knitr::kable(caption = "List of files to import and if they exist or not")
    stop("Fix the file path of setup files")
  }
  res$stillingar <- ovog::hv_import_stillingar(stillingar)
  
  # TEST
  # Is sample class in stillingar the same as in cruise
  
  
  ## Historical measurments ----------------------------------------------------
  # uses data in {mardata}
  coloured_print("Importing historical measurements (takes a while)", colour = "green")
  last.year <- current.year - 1
  history <- 
    sm_read_historical(years = c(1985:last.year), sample_class = current.synaflokkur)
  history$stodvar <-
    history$stodvar |> 
    dplyr::mutate(index = dplyr::case_when(!is.na(reitur) & !is.na(tognumer) & !is.na(veidarfaeri) ~ (reitur * 100 + tognumer) * 100 + veidarfaeri,
                                           .default = -1))
  
  ## Combine data ----------------------------------------------------------------
  # Only tows done this year
  coloured_print("Combine current and historical data", colour = "green")
  
  index.done <- res$stodvar |> dplyr::pull(index)
  
  # tempoarily add filter
  history$lengdir <-
    history$stodvar |> 
    dplyr::select(.file, synis_id, index) |> 
    dplyr::left_join(history$lengdir,
                     by = dplyr::join_by(.file, synis_id))
  history$numer <-
    history$stodvar |> 
    dplyr::select(.file, synis_id, index) |> 
    dplyr::left_join(history$numer,
                     by = dplyr::join_by(.file, synis_id))
  
  res$stodvar <- dplyr::bind_rows(res$stodvar, history$stodvar |> dplyr::filter(index %in% index.done))
  res$lengdir <- dplyr::bind_rows(res$lengdir, history$lengdir |> dplyr::filter(index %in% index.done) |> dplyr::select(-index))
  res$numer   <- dplyr::bind_rows(res$numer,   history$numer   |> dplyr::filter(index %in% index.done) |> dplyr::select(-index))
  
  res$stodvar <- 
    res$stodvar |> 
    dplyr::mutate(ar = lubridate::year(dags),
                  lon = dplyr::case_when(is.na(hift_lengd) ~ kastad_lengd,
                                         !is.na(kastad_lengd) & !is.na(hift_lengd) ~ (kastad_lengd + hift_lengd) / 2,
                                         .default = kastad_lengd),
                  lat = dplyr::case_when(is.na(hift_breidd) ~ kastad_breidd,
                                         !is.na(kastad_breidd) & !is.na(hift_breidd) ~ (kastad_breidd + hift_breidd) / 2,
                                         .default = kastad_breidd))
  
  ## Some tests ------------------------------------------------------------------
  if(any(is.na(res$lengdir$lengd))) {
    message("Unexpected: Some lengths are NA's, these are dropped")
    res$lengdir <-
      res$lengdir |> 
      dplyr::filter(!is.na(lengd))
  }
  
  # MUNGE ----------------------------------------------------------------------
  coloured_print("MUNGE", colour = "green")
  ## Skala með töldum
  coloured_print("Scale by counted", colour = "green")
  res$lengdir <- 
    res$lengdir |> 
    dplyr::left_join(res$numer |> 
                       dplyr::select(.file, synis_id, tegund, r),
                     by = dplyr::join_by(.file, synis_id, tegund)) |> 
    dplyr::mutate(n = n * r,
                  b = n * (0.00001 * lengd^3)) |> 
    dplyr::select(-r)
  
  coloured_print("Standardize to towlength", colour = "green")
  res$lengdir <- 
    res$lengdir |> 
    dplyr::left_join(res$stodvar |> 
                       dplyr::select(.file, synis_id, toglengd),
                     by = dplyr::join_by(.file, synis_id)) |> 
    sm_standardize_by_tow() |> 
    dplyr::select(-toglengd)
  
  ## length by year -------------------------------------------------------------
  coloured_print("Results by year and length", colour = "green")
  
  res$by.length <-
    res$stodvar |> 
    dplyr::select(.file, synis_id, ar) |> 
    dplyr::left_join(res$lengdir,
                     by = dplyr::join_by(.file, synis_id)) |> 
    dplyr::group_by(ar, tegund, lengd) |> 
    dplyr::reframe(n = sum(n, na.rm = TRUE),
                   b = sum(b, na.rm = TRUE)) |> 
    tidyr::gather(var, val, c(n, b))
  
  ### by stations --------------------------------------------------------------
  coloured_print("Results by station", colour = "green")
  
  res$by.station <-
    res$stodvar |> 
    dplyr::select(.file, synis_id, ar, index, lon, lat) |> 
    dplyr::left_join(res$lengdir,
                     by = dplyr::join_by(.file, synis_id)) |> 
    dplyr::group_by(ar, index, lon, lat, tegund) |> 
    dplyr::reframe(n = sum(n),
                   b = sum(b)) |> 
    # Cross all possible `species` values with the unique pairs of
    # `(ar, index)` that already exist in the data. lon and lat
    # come in for free
    # NOTE: the currently taken tows (read: index) may not have been
    #       taken in previous years. they are not many but they are
    #       may affect the calculations slightly
    tidyr::complete(tegund, tidyr::nesting(ar, index, lon, lat),
                    fill = list(n = 0, b = 0)) |> 
    tidyr::gather(var, val, c(n, b))
  
  # Lögun ----------------------------------------------------------------------
  coloured_print("Last 20 tows", colour = "green")
  # Logun last 20
  res$last.20 <-
    res$stodvar |>  
    dplyr::filter(ar == current.year) |> 
    dplyr::arrange(leidangur, dplyr::desc(togbyrjun)) |> 
    dplyr::group_by(leidangur) |> 
    dplyr::slice(1:20) |> 
    dplyr::mutate(id = dplyr::n():1) |> 
    dplyr::ungroup() |> 
    dplyr::select(leidangur, id, index) |> 
    dplyr::left_join(res$stodvar |> 
                       dplyr::select(index, ar, larett_opnun, lodrett_opnun,
                                     botnhiti, yfirbordshiti, vir_uti),
                     by = dplyr::join_by(index)) |>
    tidyr::gather(variable, value, larett_opnun:vir_uti) |> 
    dplyr::filter(!is.na(value))
  
  # Timetrend ------------------------------------------------------------------
  coloured_print("Tows and temperature - time series", colour = "green")
  # Here first get the index for the current leidangur then join by index
  #  so all past data have current leidangur associated with the current
  #  intex
  res$timetrend <-
    res$stodvar |>
    dplyr::select(.file, synis_id, ar, 
                  larett_opnun, lodrett_opnun,
                  botnhiti, yfirbordshiti, vir_uti) |> 
    tidyr::gather(variable, value, larett_opnun:vir_uti)
  
  missing <- 
    res$timetrend |> 
    dplyr::filter(ar == current.year) |> 
    dplyr::filter(is.na(value))
  if(nrow(missing) > 1) {
    coloured_print(paste0("Unexpected - missing values\n"), colour = "red")
    coloured_print("Missing values will be dropped\n", colour = "red")
    missing |> knitr::kable(caption = "List of variables with missing values:")
  }
  
  res$timetrend <- 
    res$timetrend |> 
    dplyr::filter(!is.na(value))
  
  # tidy stillingar ------------------------------------------------------------
  coloured_print("Tidy 'Stillingar' ", colour = "green")
  ## LW ------------------------------------------------------------------------
  coloured_print("'Length-weight' ", colour = "green")
  res$qc <- list()
  res$qc$lw <- 
    ovog::hv_tidy_length_weights(res$stillingar) |> 
    dplyr::select(tegund, lengd, osl1, osl2, sl1, sl2)
  res$qc$range <- 
    ovog::hv_tidy_range(res$stillingar, long = FALSE)
  
  # NOTE: FIX UPSTREAM IN ovog-package, include in "kvarnir" upfront
  #       Think we need cruise, tow number and otolith number
  res$kv.this.year <-
    res$kvarnir |>
    dplyr::left_join(res$stodvar |>
                       dplyr::select(.file, synis_id, index, leidangur, stod),
                     by = dplyr::join_by(.file, synis_id))
  
  coloured_print("Checking measurments ", colour = "green")
  coloured_print("Checking length vs weights", colour = "green")
  res$kv.this.year <-
    res$kv.this.year |> 
    dplyr::left_join(res$qc$lw, 
                     by = dplyr::join_by(tegund, lengd)) |> 
    dplyr::arrange(tegund, lengd) |> 
    dplyr::mutate(.l_osl = dplyr::if_else(dplyr::between(oslaegt, osl1, osl2), "ok", "check", "na"),
                  .l_sl  = dplyr::if_else(dplyr::between(slaegt,   sl1,  sl2), "ok", "check", "na")) |> 
    dplyr::select(-c(osl1, osl2, sl1, sl2))
  
  coloured_print("Checking gutted, gonads, liver vs ungutted ratio", colour = "green")
  coloured_print("                REMINDER: Need to get data on magi", colour = "cyan")
  res$kv.this.year <- 
    res$kv.this.year |> 
    dplyr::left_join(res$qc$range |> 
                       dplyr::select(tegund, 
                                     g1 = kynkirtlar_low, g2 = kynkirtlar_high,
                                     l1 = lifur_low,      l2 = lifur_high,
                                     s1 = slaegt_low,     s2 = slaegt_high,
                                     m1 = magi_low,       m2 = magi_high),
                     by = dplyr::join_by(tegund)) |> 
    dplyr::mutate(
      .sl_osl      = dplyr::if_else(dplyr::between(slaegt/oslaegt,   s1, s2), "ok", "check", "na"),
      .kyn = dplyr::if_else(dplyr::between(kynfaeri/oslaegt, g1, g2), "ok", "check", "na"),
      # .mag   = dplyr::if_else(dplyr::between(magi/oslaegt,,    m1, m2), "ok", "check", "na"),
      .lif   = dplyr::if_else(dplyr::between(lifur/oslaegt,    l1, l2), "ok", "check", "na")
    ) |> 
    dplyr::select(-c(g1, g2, l1, l2, s1, s2, m1, m2))
  
  
  
  # The boot -------------------------------------------------------------------
  res$boot <- 
      res |> 
      sm_boot()
  
  coloured_print("\nHURRA!", "green")
  return(res)
  
}