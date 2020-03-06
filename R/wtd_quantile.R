#' Weighted quantiles
#'
#' Compute the quantiles of \code{x} weighted by \code{weights}.
#'
#' If \code{weights} is \code{NULL}, all elements of \code{x} receive the
#' same weight. Observations with zero weights are omitted from the
#' computation. Missing values in \code{w} are not handled specially and
#' produce a missing value as the result.
#'
#' @param x a numerical vector.
#' @param probs a numerical vector with the quantile probabilities.
#' @param weights a numerical vector of weights the same length as \code{x}.
#' @param na.rm a logical value indicating wether \code{NA} values in \code{x}
#' should be stripped before the computation.
#'
#' @return The weighted quantiles of \code{x}
#'
#' @examples
#' wtd_quantile(1:5, weights = 5:1, probs = (1:4)/5)
#'
#' @family weighted statistics
#' @export
wtd_quantile <- function(x, probs, weights = NULL, na.rm = FALSE) {
  if (length(probs) == 0) {
    return(numeric(0))
  }
  if (is.null(weights)) {
    if (anyNA(x))
      return(rep(c(x[0], NA), length(probs)))
    return(stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE))
  }

  if (length(x) != length(weights))
    stop("'x' and 'weights' must have the same length")

  if (missing(probs))
    stop("Missing probabilities")
  rg <- range(probs)
  if (rg[1] < 0 | rg[2] > 1)
    stop("Probabilities out of bounds")

  if (anyNA(weights))
    return(rep(c(x[0], NA), length(probs)))

  # Observations with non-zero weight
  i <- !(weights == 0)

  # Flag NAs
  if (na.rm) {
    i <- i & !is.na(x)
  } else if (anyNA(x)) {
    return(rep(c(x[0], NA), length(probs)))
  }

  result <- Hmisc::wtd.quantile(x[i], weights = weights[i],
                                probs = probs, na.rm = FALSE)
  unname(result)
}
