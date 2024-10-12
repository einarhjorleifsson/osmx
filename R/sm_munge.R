# remotes::install_github("einarhjorleifsson/ovog")
# devtools::load_all()
# maelingar <- c("data-raw/SMH/TB2-2024.zip", "data-raw/SMH/TTH1-2024.zip")
# stillingar <- c("data-raw/SMH/stillingar_SMH_rall_(haust).zip")
# stodtoflur <- c("data-raw/SMH/stodtoflur.zip")
# current.year = lubridate::year(lubridate::today())
# res <- sm_munge(maelingar, stillingar, stodtoflur)

#' Prepares data for the smxapp
#'
#' @param maelingar Names of hafvog zip files to be imported
#' @param stillingar Name of "stillingar" zip file to be imported
#' @param stodtoflur Name of "stodtoflur" zip file to be imported
#' @param current.year The current survey year
#'
#' @return a list
#' @export
#'
sm_munge <- function(maelingar, stillingar, stodtoflur, current.year = lubridate::year(lubridate::today())) {
  
  # IMPORT --------------------------------------------------------------------
  coloured_print("IMPORT", colour = "green")
  ## Current measurments -------------------------------------------------------
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
                  hift_lengd   = -hift_lengd)
  ### Some tests ---------------------------------------------------------------
  if(any(is.na(res$stodvar$reitur))) {
    coloured_print("Reitur is missing, this may create trouble downstream", colour = "red")
    res$stodvar |> 
      dplyr::filter(is.na(reitur)) |> 
      dplyr::select(leidangur, stod, reitur) |> 
      knitr::kable(caption = "Stations with missing squares") |> 
      print()
  }
  if(any(is.na(res$stodvar$tognumer))) {
    coloured_print("Tognumer is missing, this may create trouble downstream", colour = "red")
    res$stodvar |> 
      dplyr::filter(is.na(tognumer)) |> 
      dplyr::select(leidangur, stod, tognumer) |> 
      knitr::kable(caption = "Stations with missing tow number") |> 
      print()
  }
  if(any(is.na(res$stodvar$veidarfaeri))) {
    coloured_print("Veidarfaeri missing, this may create trouble downstream", colour = "red")
    res$stodvar |> 
      dplyr::filter(is.na(veidarfaeri)) |> 
      dplyr::select(leidangur, stod, veidarfaeri) |> 
      knitr::kable(caption = "Stations with missing gear") |> 
      print()
  }
  # index text - should be unique within a cruise
  tmp <- 
    res$stodvar |> 
    dplyr::select(leidangur, stod, reitur, tognumer, veidarfaeri) |> 
    dplyr::distinct() |> 
    dplyr::group_by(leidangur, reitur, tognumer, veidarfaeri) |> 
    dplyr::mutate(n = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::filter(n > 1) |> 
    dplyr::select(-n)
  if(nrow(tmp) > 1) {
    coloured_print("Some stations have duplicate reitur-tognumer-veidarfaeri", colour = "red")
    coloured_print("This creates troubles donwstream so I will have to stop here", colour = "red")
    tmp |> 
      knitr::kable(caption = "Tables that have duplicate reitur-tognumer-veidarfaeri") |> 
      print()
    stop("Please fix in Hafvog, dump and then try again")
  }
  
  res$stodvar <- 
    res$stodvar |> 
    dplyr::mutate(index = dplyr::case_when(!is.na(reitur) & !is.na(tognumer) & !is.na(veidarfaeri) ~ (reitur * 100 + tognumer) * 100 + veidarfaeri,
                                           .default = -1))
  
  
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
  
  ## Stodtoflur ----------------------------------------------------------------
  coloured_print("Import setup ('stodtoflur')", colour = "green")
  if(!any(file.exists(stillingar))) {
    warning("At least one of the setup ('stodtoflur') files does not exist")
    tibble::tibble(file = stodtoflur,
                   exists = file.exists(stodtoflur)) |> 
      knitr::kable(caption = "List of files to import and if they exist or not")
    stop("Fix the file path of setup files")
  }
  res$stodtoflur <- ovog::hv_import_stodtoflur(stodtoflur)
  
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
  
  # temporarily add index to use as filter in next step
  history$lengdir <-
    history$stodvar |> 
    dplyr::select(leidangur, synis_id, index) |> 
    dplyr::inner_join(history$lengdir,
                      by = dplyr::join_by(leidangur, synis_id))
  history$numer <-
    history$stodvar |> 
    dplyr::select(leidangur, synis_id, index) |> 
    dplyr::inner_join(history$numer,
                      by = dplyr::join_by(leidangur, synis_id))
  
  
  res$p_capture <- 
    sm_calc_capture_probability(history)
  
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
  # NOTE: THIS SHOULD NOT BE NEEDED, CHECK WHY WE GET MISSING VALUES
  res$lengdir <-
    res$lengdir |> 
    dplyr::filter(!is.na(tegund))
  res$numer <-
    res$numer |> 
    dplyr::filter(!is.na(tegund))
  res$kvarnir <-
    res$kvarnir |> 
    dplyr::filter(!is.na(tegund))
  
  # MUNGE ----------------------------------------------------------------------
  coloured_print("MUNGE", colour = "green")
  
  ## Scale with counted --------------------------------------------------------
  coloured_print("Scale by counted", colour = "green")
  res$lengdir <- 
    res$lengdir |> 
    dplyr::left_join(res$numer |> 
                       dplyr::select(leidangur, synis_id, tegund, r),
                     by = dplyr::join_by(leidangur, synis_id, tegund)) |> 
    dplyr::mutate(n = n * r,
                  b = n * (0.00001 * lengd^3)) |> 
    dplyr::select(-r)
  
  ## Scale to standardized tow length ------------------------------------------
  coloured_print("Standardize to towlength", colour = "green")
  res$lengdir <- 
    res$lengdir |> 
    dplyr::left_join(res$stodvar |> 
                       dplyr::select(leidangur, synis_id, toglengd),
                     by = dplyr::join_by(leidangur, synis_id)) |> 
    sm_standardize_by_tow() |> 
    dplyr::select(-toglengd)
  
  ## Summarise: length by year -------------------------------------------------
  coloured_print("Results by year and length", colour = "green")
  
  res$by.length <-
    res$stodvar |> 
    dplyr::select(leidangur, synis_id, ar) |> 
    dplyr::left_join(res$lengdir,
                     by = dplyr::join_by(leidangur, synis_id)) |> 
    dplyr::group_by(ar, tegund, lengd) |> 
    dplyr::reframe(n = sum(n, na.rm = TRUE),
                   b = sum(b, na.rm = TRUE)) |> 
    tidyr::gather(var, val, c(n, b))
  
  ## Summarise: by stations ----------------------------------------------------
  coloured_print("Results by station", colour = "green")
  
  res$by.station <-
    res$stodvar |> 
    dplyr::select(leidangur, synis_id, ar, index, lon, lat) |> 
    dplyr::left_join(res$lengdir,
                     by = dplyr::join_by(leidangur, synis_id)) |> 
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
  
  res$by.rect <-
    res |> 
    sm_calc_by_square()
  
  ## Predator prey -------------------------------------------------------------
  res$pp <- 
    res$pp |> 
    dplyr::rename(sid = prey) |> 
    dplyr::left_join(res$stodtoflur$species_v |> 
                       dplyr::select(sid = species_no, prey = name),
                     by = dplyr::join_by(sid)) |> 
    dplyr::select(leidangur:astand, prey, pnr:heildarthyngd)
  
  ## Trawl metrics and temperature ---------------------------------------------
  coloured_print("\nTrawl metrics and temperature", colour = "green")
  ### Timetrend ------------------------------------------------------------------
  coloured_print("Time series", colour = "green")
  # Here first get the index for the current leidangur then join by index
  #  so all past data have current leidangur associated with the current
  #  intex
  
  # HOW CAN ONE DO THIS WITHOUT USING INDEX TOO MUCH
  tmp <- 
    res$stodvar |> 
    dplyr::filter(ar == max(ar)) |> 
    dplyr::select(leidangur, stod, index) |> 
    dplyr::arrange(leidangur, stod)
  
  res$timetrend <-
    tmp |> 
    dplyr::select(leidangur, index) |> 
    dplyr::distinct() |> 
    dplyr::left_join(res$stodvar |> 
                       dplyr::select(index, ar, larett_opnun, lodrett_opnun, 
                                     vir_uti, botnhiti, yfirbordshiti) |> 
                       dplyr::mutate(vir_uti = ifelse(ar < max(ar), vir_uti / 1.8288, vir_uti)),
                     by = dplyr::join_by(index)) |> 
    dplyr::left_join(tmp |> 
                       dplyr::select(index, stod),
                     by = dplyr::join_by(index)) |> 
    dplyr::select(leidangur, index, stod, ar, dplyr::everything()) |> 
    dplyr::arrange(leidangur, dplyr::desc(stod), dplyr::desc(ar)) |> 
    tidyr::gather(var, val, -c(leidangur:ar))
  
  ### Last 20 tows -------------------------------------------------------------
  coloured_print("Last 20 tows", colour = "green")
  
  res$timetrend.20 <-
    res$timetrend |> 
    tidyr::spread(var, val) |> 
    dplyr::arrange(dplyr::desc(leidangur), dplyr::desc(ar), dplyr::desc(stod)) |> 
    dplyr::group_by(leidangur, ar) |> 
    dplyr::slice(1:20) |> 
    dplyr::ungroup() |> 
    tidyr::gather(var, val, -c(leidangur:ar))
  
  
  # QUALITY CONTROL ------------------------------------------------------------
  ## tidy stillingar -----------------------------------------------------------
  coloured_print("Tidy 'Stillingar' ", colour = "green")
  ### Length weight ------------------------------------------------------------
  coloured_print("'Length-weight' ", colour = "green")
  res$qc <- list()
  res$qc$lw <- 
    ovog::hv_tidy_length_weights(res$stillingar) |> 
    dplyr::select(tegund, lengd, osl1, osl2, sl1, sl2)
  ### Ratio relative to ungutted weight ----------------------------------------
  res$qc$range <- 
    ovog::hv_tidy_range(res$stillingar, long = FALSE)
  
  ## QC: Check length ranges ---------------------------------------------------
  
  ## QC: Check measurements ----------------------------------------------------
  coloured_print("Checking measurments ", colour = "green")
  res$kv.this.year <-
    res$kvarnir |>
    dplyr::left_join(res$stodvar |>
                       dplyr::select(leidangur, stod, index, synis_id),
                     by = dplyr::join_by(leidangur, synis_id))
  ### Checking length vs weights -----------------------------------------------
  coloured_print("Checking length vs weights", colour = "green")
  res$kv.this.year <-
    res$kv.this.year |> 
    dplyr::left_join(res$qc$lw, 
                     by = dplyr::join_by(tegund, lengd)) |> 
    dplyr::mutate(.l_osl = dplyr::if_else(dplyr::between(oslaegt, osl1, osl2), "ok", "check", "na"),
                  .l_sl  = dplyr::if_else(dplyr::between(slaegt,   sl1,  sl2), "ok", "check", "na")) |> 
    dplyr::select(-c(osl1, osl2, sl1, sl2))
  ### Checking gutted, gonads and liver vs ungutted ratio ----------------------
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
  
  ### Formatting QC table ------------------------------------------------------
  coloured_print("Formatting QC table", colour = "green")
  res$kv.this.year <- 
    res$kv.this.year |> 
    dplyr::mutate(lest = 
                    # Not the best way to find shortcut for leidangur
                    paste0(stringr::str_sub(leidangur, 1, 3) |> stringr::str_remove("-"),
                           "-",
                           stringr::str_pad(stod, pad = "0", width = 3)),
                  lestnr = paste0(lest, "-", nr)) |> 
    dplyr::select(lest, tegund, nr, lengd, oslaegt, slaegt, lifur, kynfaeri,
                  .l_osl, .l_sl, .sl_osl, .kyn, .lif,
                  dplyr::everything()) |> 
    dplyr::arrange(leidangur, stod, tegund, nr)
  
  # Surf-and-turf --------------------------------------------------------------
  coloured_print("Togfar", colour = "green")
  tmp <- 
    res$stodvar |> 
    dplyr::select(ar, leidangur, stod, reitur, tognumer, veidarfaeri, togtimi, toghradi, toglengd, 
                  lodrett_opnun, larett_opnun, vir_uti, yfirbordshiti, botnhiti,
                  kastad_lengd, kastad_breidd, hift_lengd, hift_breidd) |> 
    dplyr::mutate(.id = 1:dplyr::n()) |> 
    # one station gets dropped
    tidyr::drop_na(kastad_lengd, kastad_breidd, hift_lengd, hift_breidd)
  
  
  stations_sf <- 
    dplyr::bind_rows(
      tmp |> 
        dplyr::select(.id, kastad_lengd, kastad_breidd) |> 
        sf::st_as_sf(coords = c("kastad_lengd", "kastad_breidd"),
                     crs = 4326),
      tmp |> 
        dplyr::select(.id, hift_lengd, hift_breidd) |> 
        sf::st_as_sf(coords = c("hift_lengd", "hift_breidd"),
                     crs = 4326)
    ) |> 
    dplyr::group_by(.id) |> 
    dplyr::summarise(do_union = FALSE) |> 
    sf::st_cast("LINESTRING") |> 
    dplyr::ungroup()
  
  sf_this_year <-
    tmp |> 
    dplyr::filter(ar == max(ar)) |> 
    dplyr::left_join(stations_sf,
                     by = dplyr::join_by(.id)) |> 
    sf::st_as_sf() |> 
    dplyr::select(-.id)
  sf_older <- 
    tmp |> 
    dplyr::filter(ar < max(ar)) |> 
    dplyr::select(.id, ar) |>
    dplyr::left_join(stations_sf,
                     by = dplyr::join_by(.id)) |> 
    sf::st_as_sf()
  res$sf <- list(sf_older = sf_older, sf_this_year = sf_this_year)
  
  # ----------------------------------------------------------------------------
  coloured_print("Valblod", colour = "green")
  res$leidangrar <- 
    res$stodvar |> 
    dplyr::filter(ar == max(ar)) |> 
    dplyr::pull(leidangur) |> unique() |> sort()
  res$tegundir <-
    res$numer |> 
    dplyr::filter(leidangur %in% res$leidangrar) |> 
    dplyr::select(tegund) |> 
    dplyr::distinct() |> 
    dplyr::arrange(tegund) |> 
    dplyr::left_join(res$stodtoflur$species_v |> 
                       dplyr::select(tegund = species_no,
                                     name),
                     by = dplyr::join_by(tegund))
  res$timi <- lubridate::now()
  
  
  
  # THE BOOT ------------------------------------------------------------------
  res <- 
    res |> 
    sm_boot()

  coloured_print("\nHURRA!", "green")
  return(res)
  
}
