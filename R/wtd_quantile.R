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
  n_probs <- length(probs)
  if (n_probs == 0)
    return(numeric())

  result <- rep(NA_real_, n_probs)

  valid_probs <- !is.na(probs)
  if (!any(valid_probs))
    return(result)
  probs <- probs[valid_probs]

  checkmate::qassert(probs, "n[0, 1]")
  checkmate::qassert(x, "n")
  if (checkmate::qtest(weights, "0")) {
    if (!length(x) || anyNA(x))
      return(result)
    q <- stats::quantile(x, probs = probs, na.rm = FALSE, names = FALSE)
  } else {
    checkmate::assert_numeric(weights, len = length(x))
    if (!length(weights) || anyNA(weights))
      return(result)

    idx <- weights != 0
    if (any(!idx)) {
      x <- x[idx]
      weights <- weights[idx]
    }
    if (anyNA(x))
      return(result)

    q <- unname(Hmisc::wtd.quantile(x, weights = weights,
                                    probs = probs, na.rm = FALSE))
  }

  result[valid_probs] <- q
  result
}
