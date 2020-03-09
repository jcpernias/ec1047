#' Weighted mean
#'
#' Compute the mean of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted from the
#' computation. Missing values in \code{w} are not handled specially and
#' produce a missing value as the result.
#'
#' @param x a numerical vector.
#' @param weights a numerical vector of weights the same length as \code{x}.
#' @param na.rm a logical value indicating wether \code{NA} values in \code{x}
#' should be stripped before the computation.
#'
#' @return The weighted mean of \code{x}.
#'
#' @examples
#' wtd_mean(1:5, weights = 5:1)
#'
#' @family weighted statistics
#' @export
wtd_mean <- function(x, weights = NULL, na.rm = FALSE) {
  checkmate::qassert(na.rm, 'B1')
  checkmate::qassert(x, 'n')
  if (checkmate::qtest(weights, '0')) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sum(x) / length(x))
  }

  checkmate::assert_numeric(weights, len = length(x))

  if (any(is.na(weights)))
    return(NA_real_)

  # Drop observations with zero weights
  idx <- weights != 0
  if (any(!idx)) {
    x <- x[idx]
    weights <- weights[idx]
  }

  if (na.rm) {
    idx <- !is.na(x)
    x <- x[idx]
    weights <- weights[idx]
  }
  weights <- weights / sum(weights)
  sum(weights * x)
}
