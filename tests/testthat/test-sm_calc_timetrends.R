test_that("sm_calc_timetrends adds timetrend and timetrend.20 to res", {
  res <- sm_calc_timetrends(make_res())
  expect_true(all(c("timetrend", "timetrend.20") %in% names(res)))
})

test_that("timetrend is in long format with var and val columns", {
  res <- sm_calc_timetrends(make_res())
  expect_true(all(c("leidangur", "index", "stod", "ar", "var", "val") %in%
                    names(res$timetrend)))
})

test_that("timetrend contains the expected trawl metric variables", {
  res <- sm_calc_timetrends(make_res())
  expected_vars <- c("larett_opnun", "lodrett_opnun", "vir_uti", "botnhiti", "yfirbordshiti")
  expect_true(all(expected_vars %in% unique(res$timetrend$var)))
})

test_that("timetrend spans both years for each index", {
  res <- sm_calc_timetrends(make_res())
  years_per_index <- res$timetrend |>
    dplyr::distinct(index, ar) |>
    dplyr::count(index)
  # Each of the 2 indices should appear in both 2024 and 2025
  expect_true(all(years_per_index$n == 2L))
})

test_that("timetrend.20 has at most 20 stations per leidangur/ar/var group", {
  res <- sm_calc_timetrends(make_res())
  counts <- res$timetrend.20 |>
    dplyr::count(leidangur, ar, var)
  expect_true(all(counts$n <= 20L))
})

test_that("timetrend.20 is a subset of timetrend", {
  res <- sm_calc_timetrends(make_res())
  # Every (index, ar, var) combo in timetrend.20 must exist in timetrend
  t20_keys <- res$timetrend.20 |>
    dplyr::distinct(index, ar, var)
  tt_keys <- res$timetrend |>
    dplyr::distinct(index, ar, var)
  expect_true(all(
    dplyr::semi_join(t20_keys, tt_keys, by = c("index", "ar", "var")) |> nrow() ==
      nrow(t20_keys)
  ))
})

test_that("historic vir_uti is converted from fathoms to meters (divided by 1.8288)", {
  res <- sm_calc_timetrends(make_res())
  # In the fixture, vir_uti = 100 for all rows.
  # Historic years (ar < max(ar) = 2025) should be divided by 1.8288.
  historic_vir <- res$timetrend |>
    dplyr::filter(var == "vir_uti", ar == 2024L) |>
    dplyr::pull(val)
  expect_equal(historic_vir, rep(100 / 1.8288, length(historic_vir)))
})
