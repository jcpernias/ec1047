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

