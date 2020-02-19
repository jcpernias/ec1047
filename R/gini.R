#' Compute Gini coefficient
#'
#' Compute the Gini coefficient of the distribution
#' of \code{y}, with weights \code{w}.
#'
#' @param y numerical vector
#' @param w numerical vector with weights for each observation
#' @param v numerical scalar, degree of inequality aversion
#' @return a scalar
#' @export
gini <- function(y, w, v = 2) {
  n <- length(y)
  oidx <- order(y)
  yo <- y[oidx]

  if (missing(w)) {
    x <- (1:n) / n
    yo_mean <- mean(yo)
    C <- mean((yo - yo_mean) * (1 - x)^(v - 1))
  } else {
    wo <- w[oidx] / sum(w)
    x <- wo / 2 + c(0, cumsum(wo)[-n])
    yo_mean <- sum(yo * wo)
    C <- sum(wo * (yo - yo_mean) * (1 - x)^(v - 1))
  }
  -v * C / yo_mean
}
