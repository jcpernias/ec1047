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
  n_x <- length(x)
  if (n_x == 0)
    return(result)
  if (is.null(weights)) {
    if (anyNA(x))
      return(result)
    q <- stats::quantile(x, probs = probs, na.rm = FALSE, names = FALSE)
  } else {
    checkmate::assert_numeric(weights, len = n_x)
    if (anyNA(weights))
      return(result)

    nonzero_weights <- weights != 0
    if (!all(nonzero_weights)) {
      x <- x[nonzero_weights]
      weights <- weights[nonzero_weights]
    }
    if (length(x) == 0 || anyNA(x))
      return(result)

    q <- unname(Hmisc::wtd.quantile(x, weights = weights,
                                    probs = probs, na.rm = FALSE))
  }

  result[valid_probs] <- q
  result
}

# x <- c(2, 9, 1, 5, 10, 1, 1, 5, 7, 10, 2, 9, 10, 3, 1, 6, 7, 1, 5,
# 3, 3, 10, 1, 7, 5, 9, 9, 8, 4, 4, 5, 6, 2, 10, 6, 5, 10, 4, 10,
# 6, 8, 3, 4, 4, 8, 1, 4, 4, 10, 5)
# tbl <- unsafe_table(x)
# cw <- cumsum(tbl$w) / sum(tbl$w)
# cbind(tbl$x, tbl$w, cumsum(tbl$w), cw)
#
# Hmisc::wtd.quantile(tbl$x, tbl$w, (0:5)/5)
# Hmisc::wtd.quantile(tbl$x, 2 * tbl$w, (0:5)/5)
# Hmisc::wtd.quantile(tbl$x, (79/50)*tbl$w, (0:5)/5)
#
# quantile(x, (1:4)/5)
# unsafe_unweighted_quantile(x, (1:4)/5)

unsafe_unweighted_quantile <- function(x, probs) {
  tbl <- unsafe_table(x)
  cw <- cumsum(tbl$w) / sum(tbl$w)
  n <- length(cw)
  stats::approx(cw, tbl$x, yright = tbl$x[n], probs,
         method = 'constant', rule = 2)
}

unsafe_table <- function(x) {
  x <- sort(x)
  n_x <- length(x)
  d_x <- x[-1] != x[-n_x]
  if (sum(d_x) == n_x - 1)
    return(list(x = x, w = rep(1, n_x)))
  idx <- c(which(d_x), n_x)
  idx_1 <- c(0, idx[-length(idx)])
  list(x = x[idx], w = idx - idx_1)
}
