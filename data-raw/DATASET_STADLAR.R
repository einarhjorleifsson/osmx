## Stadlar rallstodvar ---------------------------------------------------------
library(hafvog)
library(devtools)
library(tidyverse)
zipfile <- "data-raw/stillingar_SMB_rall_(botnfiskur).zip"
files <- unzip(zipfile, list = TRUE)
stadlar_SMB <- map2(zipfile, files$Name, hafvog:::tbl_js)
nome <- files$Name |>  str_remove("hafvog.") |> str_remove(".txt")
names(stadlar_SMB) <- nome
use_data(stadlar_SMB, overwrite = TRUE)







zipfile <- "data-raw/stillingar_SMH_rall_(haust).zip"
files <- unzip(zipfile, list = TRUE)
stadlar_SMH <- map2(zipfile, files$Name, hafvog:::tbl_js)
nome <- files$Name |>  str_remove("hafvog.") |> str_remove(".txt")
names(stadlar_SMH) <- nome
use_data(stadlar_SMH, overwrite = TRUE)

zipfile <- "data-raw/stillingar_SMN_rall.zip"
files <- unzip(zipfile, list = TRUE)
stadlar_SMN <- map2(zipfile, files$Name, hafvog:::tbl_js)
nome <- files$Name |>  str_remove("hafvog.") |> str_remove(".txt")
names(stadlar_SMN) <- nome
use_data(stadlar_SMN, overwrite = TRUE)

# stodtoflur -------------------------------------------------------------------
zipfile <- "data-raw/stodtoflur.zip"
files <- unzip(zipfile, list = TRUE)
stodtoflur <- map2(zipfile, files$Name, hafvog:::tbl_js)
nome <- files$Name |>  str_remove("hafvog.") |> str_remove("_v.txt") |> str_remove(".txt")
names(stodtoflur) <- nome
use_data(stodtoflur, overwrite = TRUE)

# older code below -------------------------------------------------------------
if(FALSE) {
  # B. STADLAR -----------------------------------------------------------------
  
  
  ## rallstodvar ----
  ### json ----
  # NOTE: Only for SMB
  stadlar.rallstodvar <-
    hafvog:::js_stadla_rallstodvar() |>
    dplyr::filter(synaflokkur %in% id) |>
    dplyr::collect(n = Inf) |>
    # 2023-09-30 Quick fix
    dplyr::filter(!is.na(hift_v)) |>
    # fix an error in hift_v for SMH, should be corrected in database
    dplyr::mutate(hift_v = ifelse(hift_v == -2444550, -244455, hift_v)) |>
    dplyr::mutate(hift_n = ifelse(hift_n ==  6537038,  653703, hift_n)) |>
    geo::geoconvert(col.names = c("kastad_v", "kastad_n")) |>
    geo::geoconvert(col.names = c("hift_v",   "hift_n"))
  
  lid <- stadlar.rallstodvar$leidangur_id[[1]]
  
  ## tegundir ----
  ### json ---
  stadlar.tegundir <-
    hafvog:::js_stadla_tegund_smb() |>
    dplyr::filter(leidangur_id == lid) |>
    dplyr::arrange(tegund) |>
    tidyr::gather(variable, value, lifur_low:kynkirtlar_high) |>
    dplyr::mutate(value = value / 100) |>
    tidyr::spread(variable, value)
  
  ## length weight ----
  ### xe ----
  ### json ----
  stadlar.lw <-
    hafvog:::js_stadla_lw() |>
    dplyr::collect(n = Inf) |>
    dplyr::mutate(osl1 = osl * (1 - fravik),
                  osl2 = osl * (1 + fravik),
                  sl1 = sl * (1 - fravik),
                  sl2 = sl * (1 + fravik)) |>
    dplyr::select(tegund, lengd, osl1:sl2)
  
  ## fishtegundir ----
  ### json ----
  fisktegundir <-
    hafvog:::js_fisktegundir() |> 
    dplyr::select(tegund = species_no, heiti = name) |>
    dplyr::arrange(tegund)
  sid <-
    hafvog:::js_fisktegundir() |> 
    dplyr::select(sid = species_no, tegund = name) |>
    dplyr::arrange(tegund)
  prey_names <-
    hafvog:::js_fisktegundir() |> 
    dplyr::select(faeduhopur = food_no, lat_heiti = sci_name,
                  isl_heiti = name,
                  tegund = species_no)
  
  ## Maeliatridi ----
  ### json ----
  aid <-
    hafvog:::js_maeliatridi() |> 
    dplyr::rename(aid = id, adgerd = heiti)
  
  
  
  # Predator ----
  ## json ----
  magaastand <- 
    hafvog:::js_magaastand()
}
