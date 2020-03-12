#' Weighted median
#'
#' Compute the median of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted from the
#' computation. Missing values are not handled specially and
#' produce a missing value as the result.
#'
#' @param x a numerical vector.
#' @param weights a numerical vector of weights the same length as \code{x}.
#'
#' @return The weighted median of \code{x}.
#'
#' @examples
#' wtd_median(1:5, weights = 5:1)
#'
#' @family weighted statistics
#' @export
wtd_median <- function(x, weights = NULL) {
  wtd_quantile(x, weights, probs = 0.5)
}
