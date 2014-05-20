\name{evaldp}
\alias{evaldp}
\title{Eval a \code{\link[dplyr]{manip}} function using a string}
\usage{
evaldp(.data, .fun.name, ..., .envir = parent.frame())
}
\arguments{
  \item{.data}{a \code{\link[dplyr]{tbl}}}

  \item{.fun.name}{any \code{\link[dplyr]{manip}} function}

  \item{...}{string arguments}

  \item{.envir}{environment}
}
\description{
Eval a \code{\link[dplyr]{manip}} function using a string
}
\details{
Useful for programming with \code{\link[dplyr]{manip}}
}
\note{
It is possible that in the next release of
\code{\link[dplyr]{dplyr}} these functionalities would
appear. Then they will be removed from this package.
}
\examples{
library(dplyr)
iris \%>\% evaldp(arrange,"Sepal.Length") \%>\%
         evaldp(filter, "Sepal.Length > 5, Species=='virginica'")
}
\references{
\url{https://gist.github.com/skranz/9681509}
}
\seealso{
\code{\link[dplyr]{manip}}
}
