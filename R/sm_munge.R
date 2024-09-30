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
  
  ## Current measurements ------------------------------------------------------
  coloured_print("Reading current measurements", colour = "green")
  if(!any(file.exists(maelingar))) {
    warning("At least one of the measurments files does not exist")
    tibble::tibble(file = maelingar,
                   exists = file.exists(maelingar)) |> 
      knitr::kable(caption = "List of files to import and if they exist or not")
    stop("Fix the file path of measurement files")
  }
  res <-
    ovog::hv_read_zips(maelingar, collapse_station = TRUE) |> 
    ovog::hv_create_tables(scale = TRUE) |> 
    sm_standardize_by_tow(tow = c(2, 4, 8)) |> 
    sm_calc_coords() |> 
    sm_add_coords_to_length()
  
  current.synaflokkur <- unique(res$ST$synaflokkur)
  if(length(current.synaflokkur) > 1) {
    stop(paste("There are more than one synaflokkur in the measurement files (",
               current.synaflokkur,
               ")"))
  }
  
  # Stillingar -----------------------------------------------------------------
  coloured_print("Reading setup ('stillingar')", colour = "green")
  if(!any(file.exists(stillingar))) {
    warning("At least one of the setup ('stillingar') files does not exist")
    tibble::tibble(file = stillingar,
                   exists = file.exists(stillingar)) |> 
      knitr::kable(caption = "List of files to import and if they exist or not")
    stop("Fix the file path of setup files")
  }
  res$stillingar <- ovog::js_stillingar_all(stillingar)
  
  
  
  
  ## Historical measurments ----------------------------------------------------
  # uses data in {mardata}
  coloured_print("Reading historical measurements (takes a while)", colour = "green")
  last.year <- current.year - 1
  history <- 
    sm_read_historical(years = c(1985:last.year), sample_class = current.synaflokkur) |> 
    sm_standardize_by_tow(tow = c(2, 4, 8)) |> 
    sm_add_coords_to_length()
  
  
  
  ## Combine data ----------------------------------------------------------------
  # Only tows done this year
  coloured_print("Combining data", colour = "green")
  
  index.done <- res$ST |> dplyr::filter(ar == current.year) |> dplyr::pull(index)
  
  
  res$ST <- dplyr::bind_rows(res$ST, history$ST |> dplyr::filter(index %in% index.done))
  res$LE <- dplyr::bind_rows(res$LE, history$LE |> dplyr::filter(index %in% index.done))
  
  ## Some tests ------------------------------------------------------------------
  if(any(is.na(res$LE$lengd))) {
    message("Unexpected: Some lengths are NA's, these are dropped")
    res$LE <-
      res$LE |> 
      dplyr::filter(!is.na(lengd))
  }
  
  ## Munge -----------------------------------------------------------------------
  ### length by year -------------------------------------------------------------
  coloured_print("Data munge", colour = "green")
  
  res$by.length <-
    res$LE |> 
    dplyr::group_by(ar, tegund, lengd) |> 
    dplyr::reframe(n = sum(n),
                   b = sum(b)) |> 
    tidyr::gather(var, val, c(n, b))
  ### by stations ----------------------------------------------------------------
  res$by.station <-
    res$LE |> 
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
  coloured_print("\nLast 20 tows", colour = "green")
  # Logun last 20
  res$last.20 <-
    res$ST |>  
    dplyr::filter(ar == current.year) |> 
    dplyr::arrange(leidangur, dplyr::desc(togbyrjun)) |> 
    dplyr::group_by(leidangur) |> 
    dplyr::slice(1:20) |> 
    dplyr::mutate(id = dplyr::n():1) |> 
    dplyr::ungroup() |> 
    dplyr::select(leidangur, id, index) |> 
    dplyr::left_join(res$ST |> 
                       dplyr::select(index, ar, larett_opnun, lodrett_opnun,
                                     botnhiti, yfirbordshiti, vir_uti),
                     by = dplyr::join_by(index)) |>
    tidyr::gather(variable, value, larett_opnun:vir_uti) |> 
    dplyr::filter(!is.na(value))
  
  # Timetrend ------------------------------------------------------------------
  coloured_print("Timetrends ", colour = "green")
  # Here first get the index for the current leidangur then join by index
  #  so all past data have current leidangur associated with the current
  #  intex
  res$timetrend <-
    res$ST |> 
    dplyr::filter(index %in% index.done,
                  ar == current.year) |>
    dplyr::select(leidangur, index) |>
    dplyr::left_join(res$ST |>
                       dplyr::select(index, ar, larett_opnun, lodrett_opnun,
                                     botnhiti, yfirbordshiti, vir_uti),
                     by = dplyr::join_by(index)) |>
    tidyr::gather(variable, value, larett_opnun:vir_uti) |>
    dplyr::filter(!is.na(value))
  
  # res$ST |>  
  #   dplyr::select(index, ar, leidangur, larett_opnun, lodrett_opnun,
  #                 botnhiti, yfirbordshiti, vir_uti) |>
  #   tidyr::gather(variable, value, larett_opnun:vir_uti)
  
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
  coloured_print("'Stillingar' ", colour = "green")
  ## LW ------------------------------------------------------------------------
  coloured_print("'Length-weight' ", colour = "green")
  res$stillingar$fiskteg_lengd_thyngd <- 
    res$stillingar$fiskteg_lengd_thyngd |> 
    dplyr::rename(tegund = fisktegund_id) |> 
    dplyr::mutate(fravik = fravik/100)
  x <-
    res$stillingar$fiskteg_lengd_thyngd |> 
    dplyr::group_by(tegund) |>
    dplyr::reframe(l.max = max(lengd))
  
  res$qc <- list()
  res$qc$lw <-
    expand.grid(tegund = unique(res$stillingar$fiskteg_lengd_thyngd$tegund),
              # This is a bit too much
              lengd = 1:1500) |>
    dplyr::as_tibble() |>
    dplyr::left_join(x, by = "tegund") |>
    dplyr::filter(lengd <= l.max) |>
    dplyr::select(-l.max) |>
    dplyr::left_join(res$stillingar$fiskteg_lengd_thyngd, 
                     by = dplyr::join_by(tegund, lengd)) |>
    dplyr::arrange(tegund, -lengd) |>
    dplyr::group_by(tegund) |>
    tidyr::fill(fravik:slaegt_a) |>
    dplyr::ungroup() |>
    dplyr:: mutate(osl = oslaegt_a * lengd^oslaegt_b,
                   sl = slaegt_a * lengd^slaegt_b) |> 
    dplyr::arrange(tegund, lengd) |> 
    dplyr::mutate(osl1 = osl * (1 - fravik),
                  osl2 = osl * (1 + fravik),
                  sl1 = sl * (1 - fravik),
                  sl2 = sl * (1 + fravik)) |>
    dplyr::select(tegund, lengd, osl1:sl2)
  
  coloured_print("'Low and high' ", colour = "green")
  res$kv.this.year <-
    res$KV |>
    # Get stodvarnumer
    dplyr::left_join(res$ST |>
                       dplyr::select(synis_id, index, leidangur, stod),
                     by = dplyr::join_by(synis_id, index))  |>
    dplyr::left_join(res$qc$lw, 
                     by = dplyr::join_by(tegund, lengd)) |> 
    dplyr::mutate(ok.l.osl = dplyr::if_else(oslaegt >= osl1 & oslaegt <= osl2, TRUE, FALSE, TRUE),
                   ok.l.sl = dplyr::if_else(slaegt >= sl1 & slaegt <= sl2, TRUE, FALSE, TRUE)) |> 
    dplyr::select(-c(osl1:sl2)) |> 
    dplyr::left_join(res$stillingar$fiskteg_tegundir |> 
                     dplyr::select(tegund = fisktegund_id, kynkirtlar_high:oslaegt_vigtad_low), 
                   by = dplyr::join_by(tegund)) |> 
    dplyr::mutate(ok.sl.osl = dplyr::if_else(slaegt/oslaegt >= oslaegt_vigtad_low & slaegt/oslaegt <= oslaegt_slaegt_high, TRUE, FALSE, TRUE),
                  ok.kirtlar.osl = dplyr::if_else(kynfaeri/oslaegt >= kynkirtlar_low & kynfaeri/oslaegt <= kynkirtlar_high, TRUE, FALSE, TRUE))
  # NOTE: Here have a trouble that variable names is not dumped in json if it is NA - should create a dummy file
                  #ok.lifur.osl = dplyr::if_else(lifur/oslaegt >= lifur_low & lifur/oslaegt <= lifur_high, TRUE, FALSE, TRUE),
                  #ok.magir.osl = dplyr::if_else(magi/oslaegt  >= magi_low & magi/oslaegt <= magi_high, TRUE, FALSE, TRUE))
    #dplyr::select(-c(kynkirtlar_high:oslaegt_vigtad_low))
  
  
  
  # The boot -------------------------------------------------------------------
  if(TRUE) {
    res <- 
      res |> 
      sm_boot()
  }
  
  coloured_print("\nHURRA!", "green")
  return(res)
  
}