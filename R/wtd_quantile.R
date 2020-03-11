#' Weighted quantiles
#'
#' Compute the quantiles of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted from the
#' computation. Missing values are not handled specially and
#' produce missing values as the result.
#'
#' @param x a numerical vector.
#' @param probs a numerical vector with the quantile probabilities.
#' @param weights a numerical vector of weights the same length as \code{x}.
#'
#' @return A \code{numeric} vector with the weighted quantiles of \code{x}.
#'
#' @examples
#' wtd_quantile(1:5, weights = 5:1, probs = (1:4)/5)
#'
#' @family weighted statistics
#' @export
wtd_quantile <- function(x, probs, weights = NULL) {
  checkmate::qassert(probs, "N+[0, 1]")
  n_probs <- length(probs)
  checkmate::qassert(x, "n")
  if (checkmate::qtest(weights, "0")) {
    if (!length(x) || anyNA(x))
      return(rep(c(x[0], NA), n_probs))
    return(stats::quantile(x, probs = probs, na.rm = FALSE, names = FALSE))
  }

  checkmate::assert_numeric(weights, len = length(x))
  if (!length(weights) || anyNA(weights))
    return(rep(c(x[0], NA), n_probs))

  idx <- weights != 0
  if (any(!idx)) {
    x <- x[idx]
    weights <- weights[idx]
  }
  if (anyNA(x))
    return(rep(c(x[0], NA), n_probs))

  result <- Hmisc::wtd.quantile(x, weights = weights,
                                probs = probs, na.rm = FALSE)
  unname(result)
}
