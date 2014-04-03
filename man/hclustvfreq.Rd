\name{hclustvfreq}
\alias{hclustvfreq}
\title{Fast hierarchical, agglomerative clustering of frequency data}
\usage{
hclustvfreq(data, freq = ~freq, method = "single", metric = "euclidean",
  p = NULL)
}
\arguments{
  \item{data}{any object that can be coerced into a double
  matrix}

  \item{method}{the agglomeration method to be used. This
  must be (an unambiguous abbreviation of) one of
  "\code{single}", "\code{ward}", "\code{centroid}" or
  "\code{median}".}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights}

  \item{metric}{the distance measure to be used. This must
  be one of \code{"euclidean"}, \code{"maximum"},
  \code{"manhattan"}, \code{"canberra"}, \code{"binary"} or
  \code{"minkowski"}}

  \item{p}{parameter for the Minkowski metric.}
}
\description{
This function implements a version of the hierarchical,
agglomerative clustering
\code{\link[fastcluster]{hclust.vector}} focused on table
of frequencies.
}
\details{
Any variables in the formula are removed from the data set.

This function is an adaptation of
\code{\link[fastcluster]{hclust.vector}} to be used with
tables of frequencies.
}
\examples{
library(dplyr)
library(fastcluster)

data <- iris[,1:3,drop=FALSE]
aa <- hclust.vector(data)
af <- hclustvfreq(data, freq=~1)
all.equal(af, aa) # Equals except in some fields

data$group <- cutree(aa,3)

tt <- tablefreq(data)
bb <- hclustvfreq(tt)
tt$group <- cutree(bb,3)

all.equal(unique(tt[,-ncol(tt)]),unique(data))
}
\seealso{
\code{\link[fastcluster]{hclust.vector}},
\code{\link{smartround}}
}

