\name{clarachunk}
\alias{clarachunk}
\alias{claramerge}
\alias{clarasub}
\title{Clustering Large Chunks}
\usage{
clarasub(x, k, samples = 50)

claramerge(subclusters, k, samples = 50)
}
\arguments{
  \item{x}{data matrix or data frame, each row corresponds
  to an observation, and each column corresponds to a
  variable. All variables must be numeric. Missing values
  (NAs) are allowed.}

  \item{k}{integer, the number of clusters. It is required
  that 0 < k < n where n is the number of observations of
  each chunk (i.e., n = nrow(x)).}

  \item{samples}{integer, number of samples to be drawn
  from the dataset.}

  \item{subclusters}{list of objects returned by clarasub}
}
\value{
A list with the following values (see
\code{\link[cluster]{clara}}): \item{n}{number of rows of
the data set.} \item{sample}{labels or case numbers of the
observations in the best sample, that is, the sample used
by the clara algorithm for the final partition.}
\item{medoids}{the medoids or representative objects of the
clusters. It is a matrix with in each row the coordinates
of one medoid.} \item{tablefreq}{a table of frequency. It
is an approximation to the number of cases in each group.}
}
\description{
Clustering data splitted in several chunks into k clusters.
}
\details{
See \code{\link[cluster]{clara}} for further details.

See Examples.
}
\note{
This function is based on \code{\link[cluster]{clara}} that
it is not available on Windows. Therefore, this
implementation does not run on Windows.
}
\examples{
if(require(cluster)){
  k <- 3

  chunk1 <- iris[1:30,1:4]
  clus1 <- clarasub(chunk1,k)

  chunk2 <- iris[-c(1:30),1:4]
  clus2 <- clarasub(chunk2,k)

  subclusters <- list(clus1, clus2)
  b <- claramerge(subclusters,k)
     print(b$medoids)

   print(nrow(b$tablefreq))
  print(b$tablefreq)
}
}
\references{
Antonio Piccolboni \code{mclust.mr}
\url{https://github.com/RevolutionAnalytics/rmr2/blob/master/pkg/examples/mclust.mr.R}
}
\seealso{
\code{\link[cluster]{clara}}, \code{\link{make.readchunk}}
}

