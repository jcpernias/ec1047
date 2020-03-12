#' Weighted mean
#'
#' Compute the mean of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted before the mean
#' is computed. Missing values are not handled specially and
#' produce a missing value as the result.
#'
#' @param x a numerical vector.
#' @param weights a numerical vector of weights the same length as \code{x}.
#'
#' @return The weighted mean of \code{x}.
#'
#' @examples
#' wtd_mean(1:5, weights = 5:1)
#'
#' @family weighted statistics
#' @export
wtd_mean <- function(x, weights = NULL) {
  checkmate::qassert(x, 'n')
  n_x <- length(x)
  if (n_x == 0)
    return(NA_real_)

  if (is.null(weights))
    return(sum(x) / n_x)

  checkmate::assert_numeric(weights, len = n_x)
  if (checkmate::anyMissing(weights))
    return(NA_real_)

  nonzero_weights <- weights != 0
  if (!any(nonzero_weights))
    return(NA_real_)

  if (!all(nonzero_weights)) {
    x <- x[nonzero_weights]
    weights <- weights[nonzero_weights]
  }

  sum(weights * x / sum(weights))
}
