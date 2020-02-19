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
