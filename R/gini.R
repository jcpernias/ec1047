#' Compute Gini coefficient
#'
#' Compute the generalized Gini coefficient of the distribution
#' of \code{y} with weights \code{weights}. The parameter \code{v}
#' is the degree of risk aversion. By default, this parameter equals
#' 2 which corresponds with the original Gini coefficient.
#'
#' Observations with zero weights are discarded before the
#' computations begin.
#'
#' @param y numerical vector.
#' @param weights numerical vector of the same length as \code{y}
#' with weights for each observation.
#' @param v numerical scalar, degree of inequality aversion. It
#' should be non-negative.
#'
#' @return A numeric scalar.
#'
#' @examples
#' gini(1:5)
#' gini(1:5, weights = 5:1)
#' gini(1:5, weights = 5:1, v = 3)
#'
#' @export
gini <- function(y, weights = NULL, v = 2) {
  checkmate::qassert(y, 'n+')
  checkmate::assert_numeric(v, any.missing = FALSE, len = 1)
  if (checkmate::qtest(weights, '0'))
    return(unsafe_unweighted_gini(y, v))

  checkmate::assert_numeric(weights, len = length(y))
  if (checkmate::anyMissing(weights))
    return(NA)
  unsafe_weighted_gini(y, weights, v)
}

unsafe_unweighted_gini <- function(y, v) {
  n <- length(y)
  oidx <- order(y)
  yo <- y[oidx]
  y_mean <- mean(yo)
  z <- (yo - y_mean) / n
  x <- (1:n) / n
  -v * sum(z * (1 - x)^(v - 1)) / y_mean
}


unsafe_weighted_gini <- function(y, w, v) {
  oidx <- w != 0
  if (!all(oidx)) {
    y <- y[oidx]
    w <- w[oidx]
  }

  n <- length(y)
  oidx <- order(y)
  yo <- y[oidx]
  wo <- w[oidx] / sum(w)
  y_mean <- sum(yo * wo)
  z <- wo * (yo - y_mean)
  x <- wo / 2 + c(0, cumsum(wo)[-n])
  -v * sum(z * (1 - x)^(v - 1)) / y_mean
}
