#' Weighted mean
#'
#' Compute the mean of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted from the
#' computation. Missing values are not handled specially and
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
  if (!length(x))
    return(NA_real_)
  if (checkmate::qtest(weights, '0'))
    return(sum(x) / length(x))


  checkmate::assert_numeric(weights, len = length(x))
  if(checkmate::anyMissing(weights))
    return(NA_real_)

  idx <- weights != 0
  if (any(!idx)) {
    x <- x[idx]
    weights <- weights[idx]
    if (!length(x))
      return(NA_real_)
  }

  weights <- weights / sum(weights)
  sum(weights * x)
}
