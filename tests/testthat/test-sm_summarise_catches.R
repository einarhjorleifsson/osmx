test_that("sm_summarise_catches adds the four expected output tables", {
  res <- sm_summarise_catches(make_res())
  expect_true(all(c("by.length", "by.station", "by.rect", "pp") %in% names(res)))
})

# --- by.length ---------------------------------------------------------------

test_that("by.length has the expected column structure", {
  res <- sm_summarise_catches(make_res())
  expect_named(res$by.length, c("ar", "tegund", "lengd", "var", "val"),
               ignore.order = TRUE)
  expect_setequal(unique(res$by.length$var), c("n", "b"))
})

test_that("by.length contains no negative or NA values", {
  res <- sm_summarise_catches(make_res())
  expect_false(any(is.na(res$by.length$val)))
  expect_true(all(res$by.length$val >= 0))
})

test_that("by.length n totals are correctly scaled by r and standardized by tow", {
  # 2024, species 1:
  #   station 1 (toglengd=4): lengd=40 n=10*r2=20; lengd=50 n=5*r2=10 → std unchanged
  #   station 2 (toglengd=2): lengd=45 n=8*r1.5=12 → std 12*(4/2)=24
  #   expected sum = 20 + 10 + 24 = 54
  res <- sm_summarise_catches(make_res())
  total <- res$by.length |>
    dplyr::filter(ar == 2024L, tegund == 1L, var == "n") |>
    dplyr::pull(val) |>
    sum()
  expect_equal(total, 54.0)
})

test_that("by.length biomass b uses the length-cubed rule", {
  # Two stations (different lon/lat) each with one fish of length 40, n=1, r=1,
  # toglengd=4. Expected b per station = 0.00001 * 40^3 = 0.64; total = 1.28.
  # Two distinct coordinates are required so that grade() receives a non-empty range.
  fixture <- make_res()
  fixture$stodvar <- tibble::tibble(
    leidangur     = c("X1-2024", "X1-2024"),
    synis_id      = c(1L, 2L),
    ar            = c(2024L, 2024L),
    index         = c(3190173L, 3190273L),
    lon           = c(-22.0, -23.0),
    lat           = c(65.0, 66.0),
    toglengd      = c(4.0, 4.0)
  )
  fixture$lengdir <- tibble::tibble(
    leidangur = c("X1-2024", "X1-2024"),
    synis_id  = c(1L, 2L),
    tegund    = c(1L, 1L),
    lengd     = c(40.0, 40.0),
    n         = c(1L, 1L)
  )
  fixture$numer <- tibble::tibble(
    leidangur = c("X1-2024", "X1-2024"),
    synis_id  = c(1L, 2L),
    tegund    = c(1L, 1L),
    r         = c(1.0, 1.0)
  )
  fixture$pp <- fixture$pp[0, ]

  res <- sm_summarise_catches(fixture)
  b_val <- res$by.length |>
    dplyr::filter(var == "b") |>
    dplyr::pull(val)
  # One lengd=40 row in by.length (both stations, same length) → total b = 2 * 0.64
  expect_equal(b_val, 2 * 0.00001 * 40^3)
})

# --- by.station --------------------------------------------------------------

test_that("by.station has the expected column structure", {
  res <- sm_summarise_catches(make_res())
  expect_named(res$by.station, c("ar", "index", "lon", "lat", "tegund", "var", "val"),
               ignore.order = TRUE)
})

test_that("by.station fills zeros for species absent at a station", {
  # Species 2 has no measurements at station index 3190273 in either year.
  # tidyr::complete should insert rows with val=0 for those combos.
  res <- sm_summarise_catches(make_res())
  absent <- res$by.station |>
    dplyr::filter(index == 3190273L, tegund == 2L, var == "n")
  expect_true(nrow(absent) > 0)
  expect_true(all(absent$val == 0))
})

test_that("by.station n matches by.length aggregated by station-year", {
  res <- sm_summarise_catches(make_res())
  # Station 1 (index=3190173), 2025, species 1:
  #   synis_id 3: lengd=42 n=12*r2=24; lengd=52 n=6*r2=12 → toglengd=4, no std change
  #   expected n = 36
  val <- res$by.station |>
    dplyr::filter(ar == 2025L, index == 3190173L, tegund == 1L, var == "n") |>
    dplyr::pull(val)
  expect_equal(val, 36.0)
})

# --- by.rect -----------------------------------------------------------------

test_that("by.rect aggregates by ICES grid cell (lon, lat rounded)", {
  res <- sm_summarise_catches(make_res())
  expect_true(all(c("ar", "lon", "lat", "tegund", "var", "val") %in% names(res$by.rect)))
  # Grid-binned coordinates must differ from station-level coordinates
  expect_true(nrow(res$by.rect) <= nrow(res$by.station))
})

# --- pp ----------------------------------------------------------------------

test_that("pp prey column is replaced with species name after join", {
  res <- sm_summarise_catches(make_res())
  expect_true("prey" %in% names(res$pp))
  # species_no 2 → "Ýsa"
  expect_equal(res$pp$prey[[1]], "Ýsa")
})

test_that("pp retains heildarthyngd column", {
  res <- sm_summarise_catches(make_res())
  expect_true("heildarthyngd" %in% names(res$pp))
  expect_equal(res$pp$heildarthyngd[[1]], 5.0)
})
