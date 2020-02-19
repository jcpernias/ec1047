#' Compute Gini coefficient
#'
#' Compute the Gini coefficient of the distribution
#' of \code{y}, with weights \code{w}.
#'
#' @param y numerical vector
#' @param w numerical vector with weights for each observation
#' @return a scalar
#' @export
gini <- function(y, w) {
  n <- length(y)
  oidx <- order(y)
  yo <- y[oidx]

  if (missing(w)) {
    x <- (1:n) / n
    yo_mean <- mean(yo)
    C <- mean((yo - yo_mean) * x)
  } else {
    wo <- w[oidx] / sum(w)
    x <- wo / 2 + c(0, cumsum(wo)[-n])
    yo_mean <- sum(yo * wo)
    C <- sum(wo * (yo - yo_mean) * x)
  }
  2 * C / yo_mean
}
