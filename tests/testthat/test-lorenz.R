test_that("perfect equality", {
  n <- 5
  y <- rep(10, n)

  # No weights
  L <- lorenz(y)
  eq <- (0:n) / n
  expect_equal(L$x, eq)
  expect_equal(L$y, eq)

  # Weights
  w <- 1:n
  L <- lorenz(y, w)
  eq <- cumsum(c(0, w)) / sum(w)
  expect_equal(L$x, eq)
  expect_equal(L$y, eq)
})
