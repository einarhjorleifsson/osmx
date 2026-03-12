test_that("standard 4-mile tow leaves n and b unchanged", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 4.0)
  result <- sm_standardize_by_tow(df)
  expect_equal(result$n, 10.0)
  expect_equal(result$b, 100.0)
})

test_that("2-mile tow doubles n and b when standardizing to 4 miles", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 2.0)
  result <- sm_standardize_by_tow(df)
  expect_equal(result$n, 20.0)
  expect_equal(result$b, 200.0)
})

test_that("8-mile tow halves n and b", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 8.0)
  result <- sm_standardize_by_tow(df)
  expect_equal(result$n, 5.0)
  expect_equal(result$b, 50.0)
})

test_that("NA toglengd is treated as the 4-mile standard (no change)", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = NA_real_)
  result <- sm_standardize_by_tow(df)
  expect_equal(result$n, 10.0)
  expect_equal(result$b, 100.0)
})

test_that("toglengd below minimum (2) is clamped to 2 miles", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 1.0)
  result <- sm_standardize_by_tow(df)
  # Clamped to 2 miles -> 10 * (4/2) = 20
  expect_equal(result$n, 20.0)
  expect_equal(result$b, 200.0)
})

test_that("toglengd above maximum (8) is clamped to 8 miles", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 10.0)
  result <- sm_standardize_by_tow(df)
  # Clamped to 8 miles -> 10 * (4/8) = 5
  expect_equal(result$n, 5.0)
  expect_equal(result$b, 50.0)
})

test_that("toglengd column is removed from output", {
  df <- tibble::tibble(n = 10.0, b = 100.0, toglengd = 4.0)
  result <- sm_standardize_by_tow(df)
  expect_true("toglengd" %in% names(result))  # standardize does not drop it
})

test_that("vectorized input is handled correctly", {
  df <- tibble::tibble(
    n        = c(10.0, 10.0, 10.0),
    b        = c(100.0, 100.0, 100.0),
    toglengd = c(2.0, 4.0, 8.0)
  )
  result <- sm_standardize_by_tow(df)
  expect_equal(result$n, c(20.0, 10.0, 5.0))
})
