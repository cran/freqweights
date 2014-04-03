## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2014-04-12 sáb 23:54 emilio on emilio-Satellite-P100>
## ============================================================




##' Create a table of frequencies
##'
##' Based on the \code{\link[plyr]{count}} function,
##' it can natively work with matrices and may be updated.
##'
##' It creates a frequency table of the \code{data}, or just of the columns specified in \code{vars}. If you provide a \code{freq} formula, the cases are weighted by the result of the formula. Any variables in the  formula are removed from the data set.
##'
##' See \code{\link[plyr]{count}} for further information. \code{\link[plyr]{count}} uses data frames; \code{tablefreq} also works with matrices.
##' @title Create a table of frequencies
##' @param data an object that can be coerced as a matrix or a data frame.  It must contain all variables in \code{vars} and in \code{freq}
##' @param vars variables to count unique values of. It may be a numeric vector or character vector 
##' @param freq a one-sided, single term formula specifying frequency weights 
##' @return A data frame or matrix with label and freq columns. When it is possible, the last column is named \code{freq} and it represents the frequency counts of the cases. This object of class \code{tablefreq}, has two attributes:
##' \item{freq}{Formula used to create the frequency table}
##' \item{colweights}{Name of the column with the weighting counts}
##' @note The author would like to thank Prof. Hadley Wickham who allowed the reutilisation of part of his code.
##' @seealso \code{\link[plyr]{count}}
##' @keywords manip
##' @importFrom plyr quickdf vaggregate
##' @importFrom data.table setnames
##' @export
##' @examples
##' tablefreq(iris[,c(1:5)])
##' tablefreq(iris, c("Sepal.Length","Species"))
##' a <- tablefreq(iris,freq=~Sepal.Length+Petal.Length)
##' head(tablefreq(a, freq=~Sepal.Width))
##' 
##' tt <- tablefreq(iris[,c(1:2,5)],freq=~Sepal.Width)
##' 
##' data <- iris[1:10,c(1:2,5)]
##' chunk1 <- iris[c(11:20),]
##' chunk2 <- iris[-c(1:20),]
##' a <- tablefreq(data,freq=~Sepal.Width)
##' a <- update(a,chunk1)
##' a <- update(a,chunk2)
##' all.equal(a, tt)
##'
##' \dontrun{
##' ##
##' ## Graphs
##' ##
##' if(require(ggplot2) && require(hflights)){
##'   library(dplyr)
##' 
##'   ## One variable
##'   ## Bar plot
##'   tt <- as.data.frame(tablefreq(hflights[,"ArrDelay"]))
##'   p <- ggplot() + geom_bar(aes(x=x, y=freq), data=tt, stat="identity")
##'   print(p)
##' 
##'   ## Histogram
##'   p <- ggplot() + geom_histogram(aes(x=x, weight= freq), data = tt)
##'   print(p)
##' 
##'   ## Density
##'   tt <- tt[complete.cases(tt),] ## remove missing values
##'   tt$w <- tt$freq / sum(tt$freq) ## weights must sum 1
##'   p <- ggplot() + geom_density(aes(x=x, weight= w), data = tt)
##'   print(p)
##' 
##'   ##
##'   ## Two distributions
##'   ##
##'   ## A numeric and a factor variable
##'   td <- tablefreq(hflights[,c("TaxiIn","Origin")])
##'   td <- td[complete.cases(td),]
##' 
##'   ## Bar plot
##'   p <- ggplot() + geom_bar(aes(x=TaxiIn, weight= freq, colour = Origin),
##'                            data = td, position ="dodge")
##'   print(p)
##' 
##'   ## Density
##'   ## compute the relative frequencies for each group
##'   td <- td %.% group_by(Origin) %.%
##'                mutate( ngroup= sum(freq), wgroup= freq/ngroup)
##'   p <- ggplot() + geom_density(aes(x=TaxiIn, weight=wgroup, colour = Origin),
##'                                data = td)
##'   print(p)
##' 
##'   ## For each group, plot its values
##'   p <- ggplot() + geom_point(aes(x=Origin, y=TaxiIn, size=freq),
##'                              data = td, alpha= 0.6)
##'   print(p)
##' 
##'   ## Two numeric variables
##'   tc <- tablefreq(hflights[,c("TaxiIn","TaxiOut")])
##'   tc <- tc[complete.cases(tc),]
##'   p <- ggplot() + geom_point(aes(x=TaxiIn, y=TaxiOut, size=freq),
##'                              data = tc, color = "red", alpha=0.5)
##'   print(p)
##' 
##'   ## Two factors
##'   tf <- tablefreq(hflights[,c("UniqueCarrier","Origin")])
##'   tf <- tf[complete.cases(tf),]
##' 
##'   ## Bar plot
##'   p <- ggplot() + geom_bar(aes(x=Origin, fill=UniqueCarrier, weight= freq),
##'                            data = tf)
##'   print(p)
##' }
##' }
tablefreq <- function(data, vars=NULL, freq=~1){
  ##print(data)
  x <- checkdatafreq(data,freq)
  ##  print(x)
  ## Hacked from plyr::count
  if (!is.null(vars)) {
    df2 <- x[,vars, drop=FALSE]
  } else {
    df2 <- x[,-ncol(x), drop=FALSE]
  }
  if(is.data.frame(df2) || is.matrix(df2)) {
    nams <- colnames(df2)
    ma <- lapply(seq_len(ncol(df2)), function(i) df2[,i])
    names(ma) <- nams
  }
  id <- id(ma, drop = TRUE) ## different to plyr::count
  u_id <- !duplicated(id)
  labels <- df2[u_id, , drop = FALSE]
  labels <- labels[order(id[u_id]), , drop = FALSE]
  formulafreq <- freq
  if(length(all.vars(freq)) == 0) { ## if freq = ~ 1
    freq <- tabulate(id, attr(id, "n"))
  } else {
    freq <- vaggregate(x[,ncol(x)], id, sum, .default = 0)
  }
  if(is.data.frame(data)) {
    ## If weights are integers, the freq is also integer
    class(freq) <- class(x[, ncol(x)])
    x <- data.frame(labels, freq)
  } else {
    x <- cbind(labels, freq)
  }
  rownames(x) <- NULL
  attr(x,"freq") <- formulafreq
  attr(x,"colweights") <- colnames(x)[ncol(x)]
  class(x) <- c(class(x),"tablefreq")
  return(x)
}


## ============================================================
##
## ============================================================


checkdatafreq <- function(data,freq){
  if (!inherits(freq, "formula")) {
    stop("`freq' must be a formula")
  }
  ## Convert data to a matrix or data.frame
  if(is.vector(data) && !is.list(data)) {
    ## print("is.vector")
    if(is.numeric(data)) {
      data <- as.matrix(data)
      colnames(data) <- "x"
    }  else {
      ## it is a character variable
      data <- data.frame(x = data, stringsAsFactors=FALSE)
    }
  } else if(is.factor(data))  {
    data <- as.data.frame(data)
    colnames(data) <- "x"
  } else if(is.list(data)) {
    ##print("is.list")
    if(is.numeric(data)) {
      data <- as.matrix(data)
    }  else {
      ## it is a data.frame
      data <- quickdf(data)
    }
  }
  ## Check the names
  if(is.matrix(data)){
    clnmsempty <- colnames(data)==""
    if(any(clnmsempty)) { # is a matrix
      colnames(data)[clnmsempty] <- paste0("V", paste0(colnames(data)[!clnmsempty],
                                                     collapse=""),
                                         1:sum(clnmsempty) )
    }
  }
  ## print(class(data))
  ## print(colnames(data))
  ## Estimate the weights
  if(length(all.vars(freq)) == 0){ ## case freq = ~ 1
    ##print(paste("checkdatafreq: freq is constant", deparse(freq)))
    w <-  1L
  } else {
    t <- terms(freq)
    t <- delete.response(t)
    deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L),
                                  collapse = " ") ## To deal with very long names
    listofv <- sapply(attr(t, "variables"), deparse2)[-1L]
    ##print(listofv) ## list of terms of the equation
    allvarsindata <- all(listofv %in% colnames(data))
    ## If it is possible, I avoid the model.matrix, since it requires the data.frame classes
    if( allvarsindata  && is.numeric(mdata <- as.matrix(data[, listofv,drop=FALSE]))) {
      ##print("easy formula")
      ##print(mdata)
      ## Now, we replace the variables of the formula by his position in the data set
      lhdtext <- gsub("~", " ", deparse(freq), fixed=TRUE)
      lhdtext <- paste(" ", lhdtext," ") ## add a extra space at the begin and at the end
      for( i in listofv ){
        pos <- which(colnames(mdata) == i)
        lhdtext <- gsub(paste0(" ",i," "), paste0(" mdata[,",pos,"] "),lhdtext)
      }
      w <- eval(parse(text=lhdtext))
    } else {
      ## Selecting the second column works fine because freq != ~ 1
      w <- model.matrix(freq, model.frame(freq, as.data.frame(data),na.action =na.pass))[, 2]
    }
                                        # ?is integer
  }
  ok <-  !is.na(w) & w > 0
  if(!any(ok)) {
    warning("checkdatafreq: Removed all cases")
  }
  if(!is.integer(w) &&
     is.data.frame(data) &&  ## matrix are numeric, not integer
     !any(as.logical(w[ok]%%1))) {
    class(w) <- "integer"
  }
  ## Remove the columns of the data that appears in the formula
  x <- cbind(data[ok, which(!(colnames(data) %in% all.vars(freq))), drop=FALSE],
             w[ok])
  if(ncol(x) < 1) {
    stop("checkdatafreq: wrong ncol of data after removing variables of the formula")
  }
  nams <- colnames(x)[-ncol(x)]
  if(is.data.frame(x)) {
    setnames(x, colnames(x)[ncol(x)], paste0("w",paste(nams,collapse="")) )
  } else {
    colnames(x)[ncol(x)] <- paste0("w",paste(nams,collapse=""))
  }
  return(x)
}




## ============================================================
##
## ============================================================

##' @param object a \code{tablefreq} object
##' @param ... more data
##' @method update tablefreq
##' @import dplyr
##' @export
##' @rdname tablefreq
update.tablefreq <- function(object, ...) {
  if (!inherits(object, "tablefreq"))
    stop("update.table: error")
  dots <- list(...)
  moredata <- dots[[1]]
  if(is.null(moredata)) return(object)
  x  <- checkdatafreq(moredata,attr(object,"freq"))
  ## Last column is the weighting variable. Assure that
  ## the name is the same in both data sets.
  if(is.data.frame(x)){
    setnames(x, colnames(x)[ncol(x)], attr(object,"colweights") )
  } else {
    colnames(x)[ncol(x)] <- attr(object,"colweights")
  }
  ## Now, join both data sets and calculate a new table of frequencies
  x <- tablefreq(rbind_list(object, x[, colnames(object)]),
                 freq=formula(paste("~",attr(object,"colweights"))))
  ## Preserve the original formula freq
  attr(x, "freq") <- attr(object,"freq")
  x
}



