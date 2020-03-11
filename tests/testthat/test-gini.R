test_that("perfect equality", {
  n <- 1000
  y <- rep(1000, n)
  expect_equal(gini(y), 0)
})

test_that("extreme concentration", {
  n <- 1000
  y <- c(rep(0, n - 1), n - 1)
  expect_equal(gini(y), 1 - 1 / n)
})

test_that("numerical simple example", {
  y <- 1:5
  expect_equal(gini(y), 8 / 30)
})

test_that("trivial weights", {
  y <- 1:5
  w <- rep_len(10, length(y))
  expect_equal(gini(y), gini(y, w))
})

test_that("weights", {
  y <- 1:5
  w <- c(10, 5, 7, 4, 6)
  expect_equal(gini(rep(y, times = w)), gini(y, w))
})

test_that("generalized index: numerical simple example", {
  y <- 1:5
  expect_equal(gini(y, v = 3), 0.32)
})

test_that("NA values in arguments", {
  y <- 1:5
  w <- 5:1

  y_NA <- c(1:4, NA)
  w_NA <- c(5:2, NA)
  expect_true(is.na(gini(y_NA)))
  expect_true(is.na(gini(y, w_NA)))
  expect_true(is.na(gini(y_NA, w)))
  expect_error(gini(y, w, NA))
  expect_error(gini(y, w_NA, NA))
})

test_that("zero weights", {
  y <- c(1:5, NA)
  w <- c(5:1, 0)
  expect_equal(gini(y, w), gini(y[1:5], w[1:5]))
})


