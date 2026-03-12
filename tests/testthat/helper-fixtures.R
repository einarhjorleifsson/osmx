# Builds a minimal synthetic `res` list that mirrors the structure produced by
# sm_import_current() + sm_import_setup() + sm_merge_historical().
# Contains two years (2024, 2025) and two stations per year.
make_res <- function() {

  # Stations: 4 rows — 2 historic (ar=2024), 2 current (ar=2025).
  # Station 1 is at index 3190173, station 2 at 3190273.
  # Station 2 in 2024 has a 2-mile tow (toglengd=2) to exercise standardization.
  stodvar <- tibble::tibble(
    leidangur     = c("X1-2024", "X1-2024", "X1-2025", "X1-2025"),
    synis_id      = 1:4,
    stod          = c(1L, 2L, 1L, 2L),
    dags          = as.Date(c("2024-03-01", "2024-03-01", "2025-03-01", "2025-03-01")),
    ar            = c(2024L, 2024L, 2025L, 2025L),
    reitur        = c(319L, 319L, 319L, 319L),
    smareitur     = c(1L, 2L, 1L, 2L),
    tognumer      = c(1L, 2L, 1L, 2L),
    veidarfaeri   = c(73L, 73L, 73L, 73L),
    index         = c(3190173L, 3190273L, 3190173L, 3190273L),
    lon           = c(-22.0, -22.5, -22.0, -22.5),
    lat           = c(65.0,  65.5,  65.0,  65.5),
    toglengd      = c(4.0, 2.0, 4.0, 4.0),
    togtimi       = c(60.0, 60.0, 60.0, 60.0),
    toghradi      = c(3.5, 3.5, 3.5, 3.5),
    kastad_lengd  = c(-22.1, -22.6, -22.1, -22.6),
    kastad_breidd = c(64.9,  65.4,  64.9,  65.4),
    hift_lengd    = c(-21.9, -22.4, -21.9, -22.4),
    hift_breidd   = c(65.1,  65.6,  65.1,  65.6),
    lodrett_opnun = c(3.0, 3.0, 3.0, 3.0),
    larett_opnun  = c(60.0, 60.0, 60.0, 60.0),
    vir_uti       = c(100L, 100L, 100L, 100L),
    botnhiti      = c(7.0, 7.0, 7.5, 7.5),
    yfirbordshiti = c(6.5, 6.5, 7.0, 7.0)
  )

  # Length frequencies.
  # Species 2 only appears at station 1 (synis_id 1 and 3);
  # station 2 (synis_id 2 and 4) has only species 1.
  # This lets tests verify that tidyr::complete fills zeros for missing combos.
  lengdir <- tibble::tibble(
    leidangur = c("X1-2024", "X1-2024", "X1-2024",
                  "X1-2024",
                  "X1-2025", "X1-2025", "X1-2025",
                  "X1-2025"),
    synis_id  = c(1L, 1L, 1L,
                  2L,
                  3L, 3L, 3L,
                  4L),
    tegund    = c(1L, 1L, 2L,
                  1L,
                  1L, 1L, 2L,
                  1L),
    lengd     = c(40.0, 50.0, 30.0,
                  45.0,
                  42.0, 52.0, 32.0,
                  47.0),
    n         = c(10L, 5L, 3L,
                  8L,
                  12L, 6L, 4L,
                  9L)
  )

  # Raising factors.
  # r=2 for species 1 at station 1 both years; r=1.5 at station 2.
  numer <- tibble::tibble(
    leidangur = c("X1-2024", "X1-2024", "X1-2024", "X1-2025", "X1-2025", "X1-2025"),
    synis_id  = c(1L,  1L,  2L,  3L,  3L,  4L),
    tegund    = c(1L,  2L,  1L,  1L,  2L,  1L),
    r         = c(2.0, 1.0, 1.5, 2.0, 1.0, 1.5)
  )

  # Predator-prey table.
  # Column order matches what sm_summarise_catches() expects:
  #   leidangur:astand — then prey (int, will be renamed sid) — then pnr:heildarthyngd
  pp <- tibble::tibble(
    leidangur     = "X1-2025",
    synis_id      = 3L,
    stod          = 1L,
    fiskur        = 1L,
    fnr           = 1L,
    flengd        = 50.0,
    oslaegt       = 1000.0,
    astand        = 1.0,
    prey          = 2L,          # species_no; will be renamed to sid for the join
    pnr           = 1L,
    lengd         = NA_real_,
    n             = NA_integer_,
    thyngd        = NA_real_,
    kyn           = NA_real_,
    heildarthyngd = 5.0
  )

  # Species lookup used by pp join.
  stodtoflur <- list(
    species_v = tibble::tibble(
      species_no = c(1L, 2L),
      name       = c("Þorskur", "Ýsa")
    )
  )

  # Planned station coordinates (handbook tows) used by sm_build_sf().
  stillingar <- list(
    sti_rallstodvar = tibble::tibble(
      leidangur_id = 1L,
      reitur       = 319L,
      smareitur    = 1L,
      tognumer     = 1L,
      lon1         = -22.2,
      lat1         =  64.8,
      lon2         = -21.8,
      lat2         =  65.2
    )
  )

  list(
    stodvar    = stodvar,
    lengdir    = lengdir,
    numer      = numer,
    pp         = pp,
    stodtoflur = stodtoflur,
    stillingar = stillingar
  )
}
