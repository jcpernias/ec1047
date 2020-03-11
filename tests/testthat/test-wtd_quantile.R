test_that("no weights", {
  x <- c(1, 2, 3, 4, 5)
  p <- (1:4)/5

  expected <- unname(stats::quantile(x, p))
  expect_equal(wtd_quantile(x, probs = p), expected)
})

test_that("weights", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  p <- (1:4)/5

  expected <- unname(stats::quantile(rep(x, times = w), p))
  expect_equal(wtd_quantile(x, probs = p, weights = w),
               expected)
})

test_that("weights length does not match wih x", {
  x <- c(1, 2, 3, 4, 5)
  p <- (1:4)/5

  expect_error(wtd_quantile(x, probs = p, weights = 1))
})

test_that("zero weight", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0, 4, 3, 2, 1)
  p <- (1:4)/5

  expect_equal(wtd_quantile(x, probs = p, weights = w),
               wtd_quantile(x[-1], probs = p, weights = w[-1]))
})

test_that("NA values in x", {
  x <- c(NA, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  p <- (1:4)/5

  expect_true(all(is.na(wtd_quantile(x, probs = p, weights = w))))
})

test_that("NA values in w", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(NA, 4, 3, 2, 1)
  p <- (1:4)/5
  expect_true(all(is.na(wtd_quantile(x, probs = p, weights = w))))
})
