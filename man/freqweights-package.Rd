\docType{package}
\name{freqweights-package}
\alias{freqweights}
\alias{freqweights-package}
\title{Working with frequency tables}
\description{
The frequency of a particular data value is the number of
times it occurs. A frequency table is a table of values
with their corresponding frequencies. Frequency weights are
integer numbers that indicate how many cases each case
represents. This package provides some functions to work
with such type of collected data.
}
\details{
\tabular{ll}{ Package: \tab freqweights\cr Type: \tab
Package\cr Version: \tab 0.0.1\cr Date: \tab 2014-04-04\cr
License: \tab GPL 3.0\cr }
}
\examples{
tablefreq(iris)
lmfreq(Sepal.Length ~ Petal.Length, tablefreq(iris))
hclustvfreq(tablefreq(iris[,1:4]))
smartround(iris[,1:4],7,short=TRUE)
}
\author{
Emilio Torres-Manzanera

Maintainer: Emilio Torres-Manzanera <torres@uniovi.es>
}
\keyword{package}

