test_that("sm_build_sf adds a res$sf list with three named elements", {
  res <- sm_build_sf(make_res())
  expect_true("sf" %in% names(res))
  expect_named(res$sf, c("sf_older", "sf_this_year", "sf_handbok"), ignore.order = TRUE)
})

test_that("sf objects are sf-class data frames", {
  res <- sm_build_sf(make_res())
  expect_true(inherits(res$sf$sf_this_year, "sf"))
  expect_true(inherits(res$sf$sf_older,     "sf"))
  expect_true(inherits(res$sf$sf_handbok,   "sf"))
})

test_that("tow tracks have LINESTRING geometry", {
  res <- sm_build_sf(make_res())
  expect_equal(
    as.character(sf::st_geometry_type(res$sf$sf_this_year, by_geometry = FALSE)),
    "LINESTRING"
  )
  expect_equal(
    as.character(sf::st_geometry_type(res$sf$sf_older, by_geometry = FALSE)),
    "LINESTRING"
  )
  expect_equal(
    as.character(sf::st_geometry_type(res$sf$sf_handbok, by_geometry = FALSE)),
    "LINESTRING"
  )
})

test_that("sf_this_year contains only the most recent year", {
  res <- sm_build_sf(make_res())
  expect_true(all(res$sf$sf_this_year$ar == max(res$sf$sf_this_year$ar)))
})

test_that("sf_older contains only years before the current year", {
  res <- sm_build_sf(make_res())
  max_ar <- max(make_res()$stodvar$ar)
  expect_true(all(res$sf$sf_older$ar < max_ar))
})

test_that("fixture produces the expected station counts", {
  res <- sm_build_sf(make_res())
  # 2 stations in 2025 → sf_this_year has 2 rows
  # 2 stations in 2024 → sf_older has 2 rows
  # 1 planned handbook station → sf_handbok has 1 row
  expect_equal(nrow(res$sf$sf_this_year), 2L)
  expect_equal(nrow(res$sf$sf_older),     2L)
  expect_equal(nrow(res$sf$sf_handbok),   1L)
})

test_that("stations with missing endpoint coordinates are silently dropped", {
  fixture        <- make_res()
  fixture$stodvar$hift_lengd[1] <- NA  # invalidate one historic station
  res <- sm_build_sf(fixture)
  # One of the two 2024 stations is now dropped
  expect_equal(nrow(res$sf$sf_older), 1L)
  # Current-year stations are unaffected
  expect_equal(nrow(res$sf$sf_this_year), 2L)
})

test_that("handbook stations with missing coordinates are silently dropped", {
  fixture <- make_res()
  fixture$stillingar$sti_rallstodvar$lon1 <- NA
  res <- sm_build_sf(fixture)
  expect_equal(nrow(res$sf$sf_handbok), 0L)
})

test_that("sf objects use WGS84 (EPSG:4326) CRS", {
  res <- sm_build_sf(make_res())
  expect_equal(sf::st_crs(res$sf$sf_this_year)$epsg, 4326L)
  expect_equal(sf::st_crs(res$sf$sf_handbok)$epsg,   4326L)
})
