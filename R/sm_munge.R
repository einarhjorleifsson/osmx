#remotes::install_github("einarhjorleifsson/ovog")
#devtools::load_all()
#cruises <- c("TB2-2024", "TTH1-2024")
#maelingar <- c("data-raw/SMH/TB2-2024.zip", "data-raw/SMH/TTH1-2024.zip")
#stillingar <- c("data-raw/SMH/stillingar_SMH_rall_(haust).zip")

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
    ovog::hv_read_cruise(maelingar, collapse_station = TRUE) |> 
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
  res$stillingar <- ovog::hv_read_stillingar(stillingar)
  
  # TEST
  # Is sample class in stillingar the same as in cruise
  
  
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
  coloured_print("Tidy 'Stillingar' ", colour = "green")
  ## LW ------------------------------------------------------------------------
  coloured_print("'Length-weight' ", colour = "green")
  res$qc <- list()
  res$qc$lw <- 
    ovog::hv_tidy_length_weights(res$stillingar) |> 
    dplyr::select(tegund, lengd, osl1, osl2, sl1, sl2)
  res$qc$range <- 
    ovog::hv_tidy_range(res$stillingar)

  coloured_print("Checking measurments ", colour = "green")
  coloured_print("Checking length vs weights", colour = "green")
  res$kv.this.year <-
    res$KV |>
    # NOTE: FIX UPSTREAM IN ovog-package, include in "kvarnir" upfront
    #       Think we need cruise, tow number and otolith number
    dplyr::left_join(res$ST |>
                       dplyr::select(synis_id, index, leidangur, stod),
                     by = dplyr::join_by(synis_id, index))  |>
    dplyr::left_join(res$qc$lw, 
                     by = dplyr::join_by(tegund, lengd)) |> 
    dplyr::mutate(ok.l.osl = dplyr::if_else(dplyr::between(oslaegt, osl1, osl2), TRUE, FALSE, TRUE),
                  ok.l.sl  = dplyr::if_else(dplyr::between(slaegt,   sl1,  sl2), TRUE, FALSE, TRUE),
                  stod_knr = paste0(stod, "-", nr)) |> 
    dplyr::select(-c(osl1:sl2))
  
  coloured_print("Checking gutted, gonads, liver vs ungutted ratio", colour = "green")
  coloured_print("                REMINDER: Need to get data on magi", colour = "cyan")
  res$kv.this.year <- 
    res$kv.this.year |> 
    dplyr::left_join(res$stillingar$fiskteg_tegundir |> 
                       dplyr::select(tegund = fisktegund_id, tidyselect::ends_with("_low"), tidyselect::ends_with("_high")) |> 
                       # NOT SURE IF THIS IS RIGHT
                       dplyr::mutate(oslaegt_vigtad_low = oslaegt_vigtad_low / 100,
                                     oslaegt_slaegt_high = oslaegt_slaegt_high / 100),
                     by = dplyr::join_by(tegund)) |> 
    dplyr::mutate(
      ok.sl.osl      = dplyr::if_else(dplyr::between(slaegt/oslaegt,   oslaegt_slaegt_low, oslaegt_slaegt_high), TRUE, FALSE, TRUE),
      ok.kirtlar.osl = dplyr::if_else(dplyr::between(kynfaeri/oslaegt,     kynkirtlar_low,     kynkirtlar_high), TRUE, FALSE, TRUE),
      # ok.magir.osl   = dplyr::if_else(dplyr::between(magi/oslaegt,               magi_low,           magi_high), TRUE, FALSE, TRUE),
      ok.lifur.osl   = dplyr::if_else(dplyr::between(lifur/oslaegt,             lifur_low,          lifur_high), TRUE, FALSE, TRUE)) |> 
    dplyr::select(-c(tidyselect::ends_with("_low"), tidyselect::ends_with("_high")))
  
  
  
  # The boot -------------------------------------------------------------------
  if(TRUE) {
    res <- 
      res |> 
      sm_boot()
  }
  
  coloured_print("\nHURRA!", "green")
  return(res)
  
}