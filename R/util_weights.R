#' Weighted mean
#'
#' Compute the weighted mean of \code{x}.
#' If \code{w} is missing, all elements of \code{x} receive the same weight.
#'
#' @param x a numerical vector.
#' @param w a numerical vector of weights the same length as \code{x}.
#' @param na.rm if \code{TRUE}, drop NAs in \code{x}
#'
#' @return the weighted mean of \code{x}
#' @noRd
w_mean <- function(x, w, na.rm = FALSE) {
  stats::weighted.mean(x, w, na.rm = na.rm)
}

#' Weighted quantiles
#'
#' Compute the weighted quantiles of \code{x}.
#' If \code{w} is missing, all elements of \code{x} receive the same weight.
#'
#' @param x a numerical vector.
#' @param w a numerical vector of weights the same length as \code{x}.
#' @param p a numerical vector with the quantile probabilities.
#' @param na.rm if \code{TRUE}, drop NAs in \code{x}
#'
#' @return the weighted mean of \code{x}
#' @noRd
w_quantile <- function(x, w, p, na.rm = FALSE) {
  if (missing(w)) {
    return(stats::quantile(x, probs = p, na.rm = na.rm, names = FALSE))
  }

  if (length(x) != length(w))
    stop("'x' and 'w' must have the same length")

  if (missing(p))
    stop("Missing probabilities")
  rg <- range(p)
  if (rg[1] < 0 | rg[2] > 1)
    stop("Probabilities out of bounds")

  # Observations with non-zero weight
  i <- !(w == 0)

  # Flag NAs
  if (na.rm) {
    i <- i & !is.na(x)
  }

  result <- Hmisc::wtd.quantile(x[i], weights = w[i],
                                probs = p, na.rm = FALSE)
  unname(result)
}


#' Weighted median
#'
#' Compute the weighted median of \code{x}.
#' If \code{w} is missing, all elements of \code{x} receive the same weight.
#'
#' @param x a numerical vector.
#' @param w a numerical vector of weights the same length as \code{x}.
#' Missing values
#' @param na.rm if \code{TRUE}, drop NAs in \code{x}
#' @return the weighted mean of \code{x}
#' @noRd
w_median <- function(x, w, na.rm = FALSE) {
  w_quantile(x, w, p = 0.5, na.rm = na.rm)
}
