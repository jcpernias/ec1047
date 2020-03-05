test_that("no weights", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(wtd_median(x), stats::median(x))
})

test_that("weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  expect_equal(wtd_median(x, weights = w),
               stats::median(rep(x, times = w)))
})

test_that("weights length does not match wih x", {
  x <- c(1, 2, 3, 4, 5)
  expect_error(wtd_median(x, weights = 1))
})

test_that("zero weight", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0, 4, 3, 2, 1)
  expect_equal(wtd_median(x, weights = w),
               wtd_median(x[-1], weights = w[-1]))
})

test_that("NA values in x", {
  x <- c(NA, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  expect_true(is.na(wtd_median(x, weights = w)))
  expect_equal(wtd_median(x, weights = w, na.rm = TRUE),
               stats::median(rep(x, times = w), na.rm = TRUE))
})

test_that("NA values in w", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(NA, 4, 3, 2, 1)
  expect_true(is.na(wtd_median(x, weights = w)))
  expect_true(is.na(wtd_median(x, weights = w, na.rm = TRUE)))
})
