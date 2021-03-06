test_that("no weights", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(wtd_mean(x), base::mean(x))
})

test_that("weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  expect_equal(wtd_mean(x, weights = w), stats::weighted.mean(x, w))
})

test_that("weights length does not match wih x", {
  x <- c(1, 2, 3, 4, 5)
  expect_error(wtd_mean(x, weights = 1))
})

test_that("zero weight", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0, 4, 3, 2, 1)
  expect_equal(wtd_mean(x, weights = w),
               wtd_mean(x[-1], weights = w[-1]))
})

test_that("NA values in x", {
  x <- c(NA, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  expect_true(is.na(wtd_mean(x, weights = w)))
})

test_that("NA values in w", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(NA, 4, 3, 2, 1)
  expect_true(is.na(wtd_mean(x, weights = w)))
})

test_that("zero weights", {
  x <- c(NA, 2, 3, 4, 5)
  w <- c(0, 4, 3, 2, 1)
  expect_equal(wtd_mean(x, weights = w), wtd_mean(x[-1], w[-1]))
  expect_true(is.na(wtd_mean(x, weights = rep(0, times = 5))))
})

test_that("zero length inputs", {
  x <- numeric()
  w <- numeric()
  expect_true(is.na(wtd_mean(x)))
  expect_true(is.na(wtd_mean(x, weights = w)))
})
