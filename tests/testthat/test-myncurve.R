library(testthat)

test_that("myncurve returns correct list values", {
  result <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)
  expected_area <- pnorm(6, mean = 10, sd = 5)
  expect_equal(result$area, expected_area, tolerance = 1e-6)
})

test_that("myncurve handles different inputs correctly", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)
  expect_equal(result$mu, 0)
  expect_equal(result$sigma, 1)
  expected_area <- pnorm(1, mean = 0, sd = 1)
  expect_equal(result$area, expected_area, tolerance = 1e-6)
})

test_that("myncurve handles edge cases", {
  result <- myncurve(mu = 0, sigma = 1, a = -5)
  expect_equal(result$area, pnorm(-5, mean = 0, sd = 1), tolerance = 1e-6)
})
