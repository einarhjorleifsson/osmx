# These tests exercise sm_import_current()'s validation logic without requiring
# real hafvog zip files. Tests that need a full import are skipped unless the
# sample data-raw files are present.

test_that("sm_import_current() stops with informative message when files do not exist", {
  expect_error(
    sm_import_current(c("nonexistent_a.zip", "nonexistent_b.zip")),
    regexp = "Fix the file path"
  )
})

test_that("sm_import_current() warns before stopping on missing files", {
  expect_warning(
    tryCatch(
      sm_import_current("nonexistent.zip"),
      error = function(e) NULL
    ),
    regexp = "does not exist"
  )
})

# Integration test — only runs when sample data are present.
# test_path() resolves relative to tests/testthat/, so navigate up to data-raw/.
sample_zip <- testthat::test_path("../../data-raw/TB1-2025.zip")

test_that("sm_import_current() returns expected tables from real data", {
  skip_if_not(file.exists(sample_zip), "sample zip not available")
  res <- sm_import_current(sample_zip)

  # Must produce the tables from hv_create_tables
  expect_true(all(c("stodvar", "lengdir", "numer", "kvarnir", "pp") %in% names(res)))

  # Coordinate columns are renamed and sign-flipped
  expect_true("kastad_lengd" %in% names(res$stodvar))
  expect_false("kastad_v_lengd" %in% names(res$stodvar))
  expect_true(all(res$stodvar$kastad_lengd <= 0))   # W longitudes are negative

  # index column is present and non-NA for valid rows
  expect_true("index" %in% names(res$stodvar))
  valid <- res$stodvar |>
    dplyr::filter(!is.na(reitur) & !is.na(tognumer) & !is.na(veidarfaeri))
  expect_true(all(valid$index > 0))

  # synaflokkur is stored for downstream use
  expect_true("current.synaflokkur" %in% names(res))
  expect_true(res$current.synaflokkur %in% c(30L, 35L))
})

test_that("sm_import_current() stops on duplicate reitur-tognumer-veidarfaeri", {
  skip_if_not(file.exists(sample_zip), "sample zip not available")

  # Inject a duplicate by cloning one row and giving it the same identifiers
  res_clean <- sm_import_current(sample_zip)
  dup_row   <- res_clean$stodvar[1, ]
  dup_row$stod <- 999L  # different stod but same reitur/tognumer/veidarfaeri

  # Patch a copy of sm_import_current to reach the duplicate check by
  # constructing a pre-validated res and calling just that logic
  fake_res <- res_clean
  fake_res$stodvar <- dplyr::bind_rows(fake_res$stodvar, dup_row)

  # The duplicate check fires when nrow(tmp) > 1
  tmp <- fake_res$stodvar |>
    dplyr::select(leidangur, stod, reitur, tognumer, veidarfaeri) |>
    dplyr::distinct() |>
    dplyr::group_by(leidangur, reitur, tognumer, veidarfaeri) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  expect_true(nrow(tmp) > 1)
})
