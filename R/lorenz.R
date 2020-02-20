#' Lorenz curve
#'
#' Compute the coordinates of the Lorenz curve for the distribution
#' of code \code{z}, using weights \code{w}. If \code{w} is not given,
#' the unwweighted Lorenz curve is computed.
#'
#' @param z a numerical vector.
#' @param w a numerical vector with the same length as \code{z}.
#' @return a [tibble][tibble::tibble-package] with the coordinates
#' of the Lorenz curve.

lorenz <- function(z, w) {
  n <- length(z)
  oidx <- order(z)
  zo <- z[oidx]

  if (missing(w)) {
    x <- (1:n) / n
    y <- cumsum(zo) / sum(zo)
  } else {
    wo <- w[oidx] / sum(w)
    x <- cumsum(wo)
    y <- cumsum(zo * wo) / sum(zo * wo)
  }

  tibble::tibble(x = c(0, x), y = c(0, y))
}
