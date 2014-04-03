## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2014-04-13 Sun 02:45 emilio on emilio-despacho>
## ============================================================


##' Smart round of variables
##'
##' If you want to reduce the number of unique elements in your data set to a specific number, you can use this function. It collects the numeric data (uni or multi-dimensional) into groups and estimates the values of the centers for each group.
##' @title Smart round of variables
##' @param x a numeric matrix or data frame. It must contains the variables in the \code{freq} formula
##' @param ndistinct number of distinct values you want to obtain. If there are missing values, you will get \code{ndistinct} + 1 distint values.
##' @param freq a one-sided, single term formula specifying frequency weights
##' @param stats statistic to compute each distinct value
##' @param method method to calculate the different groups. See \code{\link{hclustvfreq}}
##' @param short logical. If it is \code{TRUE}, the function returns the data collected in a frequency table. Otherwise, it returns a object with the same rows than the original data.
##' @return
##' If \code{short} is \code{TRUE}, it returns a frequency table of dimension \code{ndistinct}, and missing values are removed.
##'
##' Otherwise, a vector or a data frame with the same rows than the original data is returned. It preserves the original order.
##' @rdname smartround
##' @export
##' @importFrom data.table setnames
##' @importFrom plyr laply
##' @import dplyr
##' @seealso \code{\link{hclustvfreq}}
##' @examples
##' smartround(c(1:10,NA),2)
##' smartround(c(1:10,NA),2,short=TRUE)
##' smartround(iris[,1:4],7,short=TRUE)
##'
##' \dontrun{
##' if(require(hflights)){
##'
##'   x <- hflights[,c("ArrDelay")]
##'   xr <- smartround(x, 100)
##'   print(length(unique(xr))) ## 101: 100 and NA
##'   ## xr is almost equal to x
##'   print(cor(x,xr,use="pairwise.complete.obs")) # 0.998
##' 
##'   ## Now, with a 2-dimension data frame
##' 
##'   d0 <- hflights[,c("ArrDelay","DepDelay")]
##'   t0 <- tablefreq(d0)
##'   print(nrow(t0)) # there are 11046 distinct cases
##' 
##'   ## we reduce to just 100  distinct cases
##'   d2 <- smartround(d0,100)
##'   ## The correlation is greater than 0.96
##'   print(cor(cbind(d0,d2),use="pairwise.complete.obs"))
##' 
##'   print(system.time(t1 <- smartround(d0,100,short=TRUE)))
##'   ## this is fast
##'   print(system.time(tfast <- smartround(t0,100,freq=~freq, short=TRUE)))
##'   print(all.equal(t1,tfast))
##'   print(nrow(t1))
##' }
##' }
smartround <- function(x, ndistinct = 100,
                       freq=~1,
                       stats=c("mean","median"),
                       method=c("centroid", "median", "ward"),
                       short=FALSE){
  stats <- match.arg(stats, c("mean", "median"))
  method <- match.arg(method, c("centroid", "median", "ward"))
  ## Compute the table of frequencies
  tf <- tablefreq(x,freq=freq)
  freq <- formula(paste("~",attr(tf,"colweights"))) ## New freq!!
  ## drop the missing value
  tf <- tf[complete.cases(tf),,drop=FALSE]
  if(NROW(tf) < ndistinct) ndistinct <- NROW(tf)
  ## Compute the clusters/groups
  tf <- cbind(tf,
              cutree(hclustvfreq(tf,freq=freq,method=method),
                     k=ndistinct))
  ## Groups are in the last column
  clnmsempty <- colnames(tf)==""
  if(any(clnmsempty)) { # is a matrix
    colnames(tf)[clnmsempty] <- paste0("V", paste0(colnames(tf)[!clnmsempty],
                                                   collapse=""),
                                       1:sum(clnmsempty) )
  }
  ## Names and new freq
  nmsx <- colnames(tf)[ -c(ncol(tf)-1, ncol(tf))]
  nmsfreq <- colnames(tf)[c(ncol(tf)-1)]
  nmsgrp <- colnames(tf)[ncol(tf)]
  freq <- formula(paste("~", nmsfreq))
  ## Statistic to compute
  comstats <- switch(stats,
                     mean = function(data){
                       laply(nmsx, function(nms){
                         meanfreq(data[, c(nms, nmsfreq)],freq=freq)})
                     },
                     median = function(data){
                       laply(nmsx, function(nms){
                         quantilefreq(data[, c(nms, nmsfreq)],probs=c(0.5),freq=freq)})
                     })
  ## Apply it to each group,
  centergroups <- t(do.call(cbind, do(as.data.frame(tf) %.%
                                      regroup(lapply(nmsgrp, as.symbol)),
                                      function(data){
                                        c(comstats(data), unique(data[, nmsgrp]))
                                      })))
  colnames(centergroups) <- c(paste0("STATS",nmsx), nmsgrp)
  ## Join the centers with the table of frequencies
  tfwithcenters <- merge(tf, centergroups,by=nmsgrp,all.x=TRUE)
  if(short){ # we return the frequency table
    dd <- tablefreq(tfwithcenters, vars=paste0("STATS",nmsx), freq=freq )
    newnames <- c(nmsx,colnames(dd)[ncol(dd)])
    if(is.matrix(dd)) {
      colnames(dd) <- newnames
    } else {
      setnames(dd, newnames)
    }
    return(dd)
  }
  ## We need to keep the order of the data
  x <- cbind(x, seq_len(NROW(x)))
  nmsid <- paste0("id",paste0(nmsx,collapse=""))
  if(is.matrix(x)) {
    colnames(x) <- c(nmsx,nmsid )
  } else {
    setnames(x, c(nmsx, nmsid))
  }
  ## merge te data with the centers. Order is missing
  xwithcenter<- merge(x,
                      tfwithcenters,
                      by=nmsx,
                      all.x=TRUE)[, c(nmsid,paste0("STATS",nmsx))]
  setnames(xwithcenter, c(nmsid, nmsx)) # we restore the original names
  ## Now, we arrange the data, and drop the id
  ## if x is a vector, we must return a vector, not a data frame
  dots <- sapply( nmsid,function(x) substitute(x,
                                               list(x=as.name(x))))
  do.call(arrange, c(list(.data=xwithcenter), dots))[, -1, drop=TRUE]
}

