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
  expect_true(all(is.na(wtd_quantile(x, probs = p,
                                     weights = rep(0, 5)))))
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

test_that("NA values with zero weight", {
  x <- c(NA, 2, 3, 4, 5)
  w <- c(0, 4, 3, 2, 1)
  p <- 0.5
  expect_equal(wtd_quantile(x, probs = p, weights = w), 3)
})

test_that("NA probabilities", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  p <- c(0.5, NA)
  q <- wtd_quantile(x, probs = p, weights = w)
  expect_true(q[1] == 2 && is.na(q[2]))
  q <- wtd_quantile(x, probs = p)
  expect_true(q[1] == 3 && is.na(q[2]))
  q <- wtd_quantile(x, c(NA, NA))
  expect_true(length(q) == 2 && all(is.na(q)))
})

test_that("zero-length probabilities", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(5, 4, 3, 2, 1)
  p <- numeric(0)
  q <- wtd_quantile(x, probs = p, weights = w)
  expect_true(is.numeric(q) && length(q) == 0)
})
