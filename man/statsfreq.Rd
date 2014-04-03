\name{statsfreq}
\alias{corfreq}
\alias{covfreq}
\alias{meanfreq}
\alias{quantilefreq}
\alias{sdfreq}
\alias{statsfreq}
\title{Descriptive statistics of a frequency table.}
\usage{
meanfreq(data, freq = ~freq)

quantilefreq(data, probs = c(0, 0.25, 0.5, 0.75, 1), freq = ~freq)

covfreq(data, freq = ~freq)

sdfreq(data, freq = ~freq)

corfreq(data, freq = ~freq)
}
\arguments{
  \item{data}{a data frame or matrix.}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights. Default is \code{~freq}.}

  \item{probs}{A vector of quantiles to compute. Default is
  0 (min), .25, .5, .75, 1 (max).}
}
\value{
\code{meanfreq} and \code{sdfreq} return scalars.
\code{quantilefreq} returns a vector with the same length
as \code{probs}. \code{covfreq} and \code{corfreq} the
estimated covariance matrix and correlation matrix,
respectively.
}
\description{
Computes the descriptive statistics of a frequency table.
}
\details{
These functions compute various weighted versions of
standard estimators.

\code{meanfreq}, \code{sdfreq}, \code{quantilefreq},
\code{covfreq}, \code{corfreq} estimate the mean, standard
desviation, quantiles, covariances and correlation matrix,
respectively. In this last two cases, resulst are equals to
the \code{pairwise.complete.obs} option of \code{cov} and
\code{cor} of the desaggregated data, respectively.

Missing values or cases with non-positive frequency weights
are automatically removed.

Data set must contain the variables that compose the
formula. These variables are removed from the data set in
order to calculate the descriptive statistics.

The algorithm of \code{quantilefreq} are based on
\code{\link[Hmisc]{wtd.quantile}}.
}
\note{
The author would like to thank Prof. Frank E. Harrell Jr.
who allowed the reutilisation of part of his code.
}
\examples{
df <- tablefreq(iris[, c("Sepal.Length","Petal.Length")])
identical(meanfreq(df[,c(1,3)]), mean(iris[,1]))
identical(sdfreq(df[,c(1,3)]), sd(iris[,c("Sepal.Length")]))
all.equal(covfreq(df), cov(iris[,c("Sepal.Length","Petal.Length")],use="pairwise.complete.obs"))
}
\references{
Andrews, Chris,
\url{https://stat.ethz.ch/pipermail/r-help/2014-March/368350.html}
}
\seealso{
\code{\link{tablefreq}}, \code{\link[Hmisc]{wtd.quantile}}
}
\keyword{univar}

