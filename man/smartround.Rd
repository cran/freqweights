\name{smartround}
\alias{smartround}
\title{Smart round of variables}
\usage{
smartround(x, ndistinct = 100, freq = ~1, stats = c("mean", "median"),
  method = c("centroid", "median", "ward"), short = FALSE)
}
\arguments{
  \item{x}{a numeric matrix or data frame. It must contains
  the variables in the \code{freq} formula}

  \item{ndistinct}{number of distinct values you want to
  obtain. If there are missing values, you will get
  \code{ndistinct} + 1 distint values.}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights}

  \item{stats}{statistic to compute each distinct value}

  \item{method}{method to calculate the different groups.
  See \code{\link{hclustvfreq}}}

  \item{short}{logical. If it is \code{TRUE}, the function
  returns the data collected in a frequency table.
  Otherwise, it returns a object with the same rows than
  the original data.}
}
\value{
If \code{short} is \code{TRUE}, it returns a frequency
table of dimension \code{ndistinct}, and missing values are
removed.

Otherwise, a vector or a data frame with the same rows than
the original data is returned. It preserves the original
order.
}
\description{
Smart round of variables
}
\details{
If you want to reduce the number of unique elements in your
data set to a specific number, you can use this function.
It collects the numeric data (uni or multi-dimensional)
into groups and estimates the values of the centers for
each group.
}
\examples{
smartround(c(1:10,NA),2)
smartround(c(1:10,NA),2,short=TRUE)
smartround(iris[,1:4],7,short=TRUE)

\dontrun{
if(require(hflights)){

  x <- hflights[,c("ArrDelay")]
  xr <- smartround(x, 100)
  print(length(unique(xr))) ## 101: 100 and NA
  ## xr is almost equal to x
  print(cor(x,xr,use="pairwise.complete.obs")) # 0.998

  ## Now, with a 2-dimension data frame

  d0 <- hflights[,c("ArrDelay","DepDelay")]
  t0 <- tablefreq(d0)
  print(nrow(t0)) # there are 11046 distinct cases

  ## we reduce to just 100  distinct cases
  d2 <- smartround(d0,100)
  ## The correlation is greater than 0.96
  print(cor(cbind(d0,d2),use="pairwise.complete.obs"))

  print(system.time(t1 <- smartround(d0,100,short=TRUE)))
  ## this is fast
  print(system.time(tfast <- smartround(t0,100,freq=~freq, short=TRUE)))
  print(all.equal(t1,tfast))
  print(nrow(t1))
}
}
}
\seealso{
\code{\link{hclustvfreq}}
}

