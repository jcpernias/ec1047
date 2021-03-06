% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wtd_mean.R
\name{wtd_mean}
\alias{wtd_mean}
\title{Weighted mean}
\usage{
wtd_mean(x, weights = NULL)
}
\arguments{
\item{x}{a numerical vector.}

\item{weights}{a numerical vector of weights the same length as \code{x}.}
}
\value{
The weighted mean of \code{x}.
}
\description{
Compute the mean of \code{x} weighted by \code{weights}.
}
\details{
If \code{weights} is \code{NULL}, all elements of \code{x} receive the
same weight. Observations with zero weights are omitted before the mean
is computed. Missing values are not handled specially and
produce a missing value as the result.
}
\examples{
wtd_mean(1:5, weights = 5:1)

}
\seealso{
Other weighted statistics: 
\code{\link{wtd_median}()},
\code{\link{wtd_quantile}()}
}
\concept{weighted statistics}
