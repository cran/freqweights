##' This function implements a version of the hierarchical, agglomerative clustering  \code{\link[fastcluster]{hclust.vector}} focused on table of frequencies.
##' 
##' Any variables in the formula are removed from the data set.
##' 
##' This function is an adaptation of \code{\link[fastcluster]{hclust.vector}} to be used with tables of frequencies.
##' @title Fast hierarchical, agglomerative clustering of frequency data
##' @param data any object that can be coerced into a double matrix
##' @param method the agglomeration method to be used. This must be (an unambiguous abbreviation of) one of "\code{single}", "\code{ward}", "\code{centroid}" or "\code{median}".
##' @param freq a one-sided, single term formula specifying frequency weights
##' @param metric the distance measure to be used. This must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"canberra"}, \code{"binary"} or \code{"minkowski"}
##' @param p parameter for the Minkowski metric.
##' @seealso \code{\link[fastcluster]{hclust.vector}}, \code{\link{smartround}}
##' @importFrom fastcluster hclust hclust.vector
##' @import dplyr
##' @export
##' @examples
##' library(dplyr)
##' library(fastcluster)
##' 
##' data <- iris[,1:3,drop=FALSE]
##' aa <- hclust.vector(data)
##' af <- hclustvfreq(data, freq=~1)
##' all.equal(af, aa) # Equals except in some fields
##'
##' data$group <- cutree(aa,3)
##'
##' tt <- tablefreq(data)
##' bb <- hclustvfreq(tt)
##' tt$group <- cutree(bb,3)
##' 
##' all.equal(unique(tt[,-ncol(tt)]),unique(data))

hclustvfreq <- function(data, freq=~freq, method="single", metric= "euclidean", p=NULL){
  x <- checkdatafreq(data,freq)
  if(NROW(x) != NROW(data) || !all(complete.cases(x)) ) {
    stop("hclustvfreq: Missing cases, or non-positive frequency weights!")
  }
  hclust.vector(as.matrix(x[,-ncol(x),drop=FALSE]), method=method, members=x[,ncol(x)], metric=metric, p=p)
}
