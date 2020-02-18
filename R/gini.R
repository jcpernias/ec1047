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
  # TODO: duplicate values in y?
  n <- length(y)
  oidx <- order(y)
  yo <- y[oidx]
  # TODO: check this hack
  if (missing(w)) {
    wo <- rep_len(1 / n, length.out = n)
  } else {
    wo <- w[oidx] / sum(w)
  }
  x <- wo / 2 + c(0, cumsum(wo)[-n])
  yo_mean <- sum(yo * wo)
  2 * sum(wo * (yo - yo_mean) * x) / yo_mean
}
