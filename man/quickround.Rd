\name{quickround}
\alias{quickround}
\title{Round data sets}
\usage{
quickround(x, accuracy)
}
\arguments{
  \item{x}{a \code{tbl} object or a numeric or POSIXct
  matrix}

  \item{accuracy}{number to round to; for POSIXct objects,
  a number of seconds}
}
\value{
A \code{tbl} object.
}
\description{
A wrapper of \code{\link[plyr]{round_any}} to round data
sets to multiple of any number.
}
\details{
The \code{x} may contain non numerical varibles (factor,
character, logical). They will remain unchanged. The
numerical or time variables will be rounded to a multiple
fo the accuracy.

If \code{accuracy} is of length 1, then this value is
applied to all the columns of the data set. Otherwise, its
length must be the same as the number of columns of
\code{x}, including non numerical variables. If any value
if \code{NA}, the corresponding variable will remain
unchanged.
}
\examples{
quickround(iris,0.2)
quickround(iris[,1:3],c(0.2,0.5,1.0))

tfq <- tablefreq(iris, vars=c("Sepal.Length","Species"))
a <- quickround(tfq, c(0.3, NA, NA))
b <- tablefreq(a, freq="freq")
b
}
\seealso{
\code{\link[plyr]{round_any}}
}

