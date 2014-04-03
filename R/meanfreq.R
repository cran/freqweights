## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2014-04-11 vie 18:30 emilio on emilio-Satellite-P100>
## ============================================================



## ============================================================
##
## ============================================================

##' Descriptive statistics of a frequency table.
##' 
##' Computes the descriptive statistics of a frequency table.
##' 
##' These functions compute various weighted versions of standard estimators.
##' 
##' \code{meanfreq}, \code{sdfreq}, \code{quantilefreq}, \code{covfreq},
##' \code{corfreq} estimate the mean, standard desviation, quantiles,
##' covariances and correlation matrix, respectively. In this last two cases,
##' resulst are equals to the \code{pairwise.complete.obs} option of \code{cov}
##' and \code{cor} of the desaggregated data, respectively.
##' 
##' Missing values or cases with non-positive frequency weights are
##' automatically removed.
##' 
##' Data set must contain the variables that compose the formula. These
##' variables are removed from the data set in order to calculate the
##' descriptive statistics.
##' 
##' The algorithm of \code{quantilefreq} are based on
##' \code{\link[Hmisc]{wtd.quantile}}.
##' 
##' @name statsfreq
##' @rdname statsfreq
##' @aliases meanfreq quantilefreq covfreq corfreq sdfreq
##' @param data a data frame or matrix.
##' @param freq a one-sided, single term formula specifying frequency weights. Default is \code{~freq}. 
##' @param probs A vector of quantiles to compute. Default is 0 (min), .25, .5,
##' .75, 1 (max).
##' @return \code{meanfreq} and \code{sdfreq} return scalars.
##' \code{quantilefreq} returns a vector with the same length as \code{probs}.
##' \code{covfreq} and \code{corfreq} the estimated covariance matrix and
##' correlation matrix, respectively.
##' @seealso 
##' \code{\link{tablefreq}}, \code{\link[Hmisc]{wtd.quantile}}
##' @references Andrews, Chris,
##' \url{https://stat.ethz.ch/pipermail/r-help/2014-March/368350.html}
##' @note The author would like to thank Prof. Frank E. Harrell Jr. who allowed the reutilisation of part of his code.
##' @keywords univar
##' @examples
##' df <- tablefreq(iris[, c("Sepal.Length","Petal.Length")])
##' identical(meanfreq(df[,c(1,3)]), mean(iris[,1]))
##' identical(sdfreq(df[,c(1,3)]), sd(iris[,c("Sepal.Length")]))
##' all.equal(covfreq(df), cov(iris[,c("Sepal.Length","Petal.Length")],use="pairwise.complete.obs"))
##' @name statsfreq
##' @rdname statsfreq
NULL

##' @rdname statsfreq
##' @export
meanfreq <- function(data, freq=~freq){
  x <- checkdatafreq(data,freq)
  ok <- complete.cases(x[,1])
  sum(x[ok,ncol(x)] * x[ok,1]) / sum(x[ok,ncol(x)])
}


##' @rdname statsfreq
##' @export
quantilefreq <-  function(data, probs = c(0, 0.25, 0.5, 0.75, 1), freq = ~freq){
  x <- checkdatafreq(data, freq)
  ok <- complete.cases(x[,1])
  x <- x[ok,c(1,ncol(x)),drop=FALSE]
  if (any(probs < 0 | probs > 1))
    stop("Probabilities must be between 0 and 1 inclusive")
  nams <- paste(format(round(probs * 100,
                             if (length(probs) > 1)
                             2 - log10(diff(range(probs)))
                             else 2)),
                "%", sep = "")
  n <- sum(x[,ncol(x)])
  order <- 1 + (n - 1) * probs
  low <- pmax(floor(order), 1)
  high <- pmin(low + 1, n)
  order <- order%%1
  allq <- approx(cumsum(x[,ncol(x)]), x[,1],
                 xout = c(low, high), method = "constant",
                 f = 1, rule = 2)$y
  k <- length(probs)
  quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
  names(quantiles) <- nams
  return(quantiles)
}


## ============================================================
##
## ============================================================


covfreqcompletecases <- function(x){
  x <- as.matrix(x)
  if (!all(is.finite(x)))
    stop("'x' must contain finite values only")
  w <- x[,ncol(x)]
  x <- x[,-ncol(x),drop=FALSE]
  n <- sum(w)
  center <-  colSums(w * x) / n
  xcw <- sqrt(w) * sweep(x, 2, center, check.margin = TRUE)
  r <- crossprod(xcw)
  if( n == 1) {
    r <- 0
  } else {
    r <- r/(n-1)
  }
  rownames(r) <- colnames(x)
  colnames(r) <- colnames(x)
  return(r)
}


checkdatacov <- function(data, freq, dotablefreq=TRUE, returnasmatrix = TRUE) {
  x <- checkdatafreq(data,freq)
  if(dotablefreq) {
    ncoloriginal <- ncol(x)
    x <- tablefreq(x, freq=formula(paste("~",colnames(x)[ncol(x)] )))
    ##   plyr::count(as.data.frame(x),wt_var=colnames(x)[ncol(x)]) ## puede haber duplicados
    if(ncol(x)> ncoloriginal) {
      x <- x[,-c(ncol(x)-1), drop=FALSE]
    }
  }
  if(returnasmatrix) {
    return(as.matrix(x))
  } else {
    return(x)
  }
}




##' @rdname statsfreq
##' @export
covfreq <- function(data, freq = ~freq) {
  x <- checkdatacov(data, freq)
  ##print(paste("covfreq a ",dim(x)))
  if( all(complete.cases(x)) ) {
    return(covfreqcompletecases(x))
  } else  {
    ##print(dim(x))
    ncy <- ncx <- ncol(x) - 1
    r <- matrix(0, nrow = ncx, ncol = ncy)
    for (i in seq_len(ncx)) {
      for (j in seq_len(i)) {
        cols <-  c(i,j,ncol(x))
        ok <- complete.cases(x[,cols])
        covb <- covfreqcompletecases(x[ok,cols, drop=FALSE])
        r[i,j] <- r[j,i] <- covb[1,2]
      }
    }
    rownames(r) <- colnames(x)[-ncol(x)]
    colnames(r) <- colnames(x)[-ncol(x)]
    return(r)
  }
}



##' @rdname statsfreq
##' @export
sdfreq <- function(data, freq=~freq){
  sqrt(covfreq(data,freq))[1,1]
}



##' @rdname statsfreq
##' @export
corfreq <- function(data, freq = ~freq){
  x <- checkdatacov(data, freq)
  ncy <- ncx <- ncol(x) -1
  r <- matrix(0, nrow = ncx, ncol = ncy)
  for (i in seq_len(ncx)) {
    for (j in seq_len(i)) {
      cols <-  c(i,j,ncol(x))
      ok <- complete.cases(x[,cols])
      covb <- covfreqcompletecases(x[ok,cols, drop=FALSE])
      r[i,j] <- r[j,i] <- covb[1,2] / sqrt(covb[1,1] * covb[2,2])
    }
  }
  rownames(r) <- colnames(x)[-ncol(x)]
  colnames(r) <- colnames(x)[-ncol(x)]
  return(r)
}




