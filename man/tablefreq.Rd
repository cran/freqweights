\name{tablefreq}
\alias{tablefreq}
\alias{update.tablefreq}
\title{Create a table of frequencies}
\usage{
tablefreq(data, vars = NULL, freq = ~1)

\method{update}{tablefreq}(object, ...)
}
\arguments{
  \item{data}{an object that can be coerced as a matrix or
  a data frame.  It must contain all variables in
  \code{vars} and in \code{freq}}

  \item{vars}{variables to count unique values of. It may
  be a numeric vector or character vector}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights}

  \item{object}{a \code{tablefreq} object}

  \item{...}{more data}
}
\value{
A data frame or matrix with label and freq columns. When it
is possible, the last column is named \code{freq} and it
represents the frequency counts of the cases. This object
of class \code{tablefreq}, has two attributes:
\item{freq}{Formula used to create the frequency table}
\item{colweights}{Name of the column with the weighting
counts}
}
\description{
Create a table of frequencies
}
\details{
Based on the \code{\link[plyr]{count}} function, it can
natively work with matrices and may be updated.

It creates a frequency table of the \code{data}, or just of
the columns specified in \code{vars}. If you provide a
\code{freq} formula, the cases are weighted by the result
of the formula. Any variables in the formula are removed
from the data set.

See \code{\link[plyr]{count}} for further information.
\code{\link[plyr]{count}} uses data frames;
\code{tablefreq} also works with matrices.
}
\note{
The author would like to thank Prof. Hadley Wickham who
allowed the reutilisation of part of his code.
}
\examples{
tablefreq(iris[,c(1:5)])
tablefreq(iris, c("Sepal.Length","Species"))
a <- tablefreq(iris,freq=~Sepal.Length+Petal.Length)
head(tablefreq(a, freq=~Sepal.Width))

tt <- tablefreq(iris[,c(1:2,5)],freq=~Sepal.Width)

data <- iris[1:10,c(1:2,5)]
chunk1 <- iris[c(11:20),]
chunk2 <- iris[-c(1:20),]
a <- tablefreq(data,freq=~Sepal.Width)
a <- update(a,chunk1)
a <- update(a,chunk2)
all.equal(a, tt)

\dontrun{
##
## Graphs
##
if(require(ggplot2) && require(hflights)){
  library(dplyr)

  ## One variable
  ## Bar plot
  tt <- as.data.frame(tablefreq(hflights[,"ArrDelay"]))
  p <- ggplot() + geom_bar(aes(x=x, y=freq), data=tt, stat="identity")
  print(p)

  ## Histogram
  p <- ggplot() + geom_histogram(aes(x=x, weight= freq), data = tt)
  print(p)

  ## Density
  tt <- tt[complete.cases(tt),] ## remove missing values
  tt$w <- tt$freq / sum(tt$freq) ## weights must sum 1
  p <- ggplot() + geom_density(aes(x=x, weight= w), data = tt)
  print(p)

  ##
  ## Two distributions
  ##
  ## A numeric and a factor variable
  td <- tablefreq(hflights[,c("TaxiIn","Origin")])
  td <- td[complete.cases(td),]

  ## Bar plot
  p <- ggplot() + geom_bar(aes(x=TaxiIn, weight= freq, colour = Origin),
                           data = td, position ="dodge")
  print(p)

  ## Density
  ## compute the relative frequencies for each group
  td <- td \%.\% group_by(Origin) \%.\%
               mutate( ngroup= sum(freq), wgroup= freq/ngroup)
  p <- ggplot() + geom_density(aes(x=TaxiIn, weight=wgroup, colour = Origin),
                               data = td)
  print(p)

  ## For each group, plot its values
  p <- ggplot() + geom_point(aes(x=Origin, y=TaxiIn, size=freq),
                             data = td, alpha= 0.6)
  print(p)

  ## Two numeric variables
  tc <- tablefreq(hflights[,c("TaxiIn","TaxiOut")])
  tc <- tc[complete.cases(tc),]
  p <- ggplot() + geom_point(aes(x=TaxiIn, y=TaxiOut, size=freq),
                             data = tc, color = "red", alpha=0.5)
  print(p)

  ## Two factors
  tf <- tablefreq(hflights[,c("UniqueCarrier","Origin")])
  tf <- tf[complete.cases(tf),]

  ## Bar plot
  p <- ggplot() + geom_bar(aes(x=Origin, fill=UniqueCarrier, weight= freq),
                           data = tf)
  print(p)
}
}
}
\seealso{
\code{\link[plyr]{count}}
}
\keyword{manip}

