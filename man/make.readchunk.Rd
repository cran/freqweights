\name{make.readchunk}
\alias{make.readchunk}
\title{Fast and friendly chunk file finagler}
\usage{
make.readchunk(input, FUN = identity, chunksize = 5000L,
  verifylastrecord = TRUE)
}
\arguments{
  \item{input}{a length 1 character string. See Details.}

  \item{FUN}{any function applicated to each chunk}

  \item{chunksize}{number of lines for each chunk}

  \item{verifylastrecord}{logical. In very rare cases, it
  is necessary to check whether the last chunk is correct.}
}
\value{
A function with an logical argument, \code{reset}. If this
argument is \code{TRUE}, it indicates that the data should
be reread from the beginning by subsequent calls. When it
reads all the data, it automatically resets the file. This
function returns the value of \code{FUN} applied to the
chunk. By default, the chunk is returned as a
\code{\link[dplyr]{tbl_df}} object.
}
\description{
Read a file chunk by chunk
}
\details{
It creates a function that reads sucesive chunks of the
data referenced by \code{input} usings the
\code{\link[data.table]{fread}} function. The \code{input}
is characterized in the help page of
\code{\link[data.table]{fread}}. The data contained in the
\code{input} reference should not have any header.

This function is inspired by the
\code{\link[biglm]{bigglm}} example.
}
\examples{
\dontrun{
library(hflights)
nrow(hflights) # Number of rows

## We create a file with no header
input <- "hflights.csv"
write.table(hflights,file=input,sep=",",
            row.names=FALSE,col.names=FALSE)

## Get the number of rows of each chunk
readchunk <- make.readchunk(input,FUN=function(x){NROW(x)})

a <- NULL
while(!is.null(b <- readchunk())) {
  if(is.null(a)) {
    a <- b
  } else {
    a <- a+b
  }
}
all.equal(a, nrow(hflights))

## It resets automatically the file
a <- NULL
while(!is.null(b <- readchunk())) {
  if(is.null(a)) {
    a <- b
  } else {
    a <- a+b
  }
}
all.equal(a, nrow(hflights))
}
}
\seealso{
\code{\link[biglm]{bigglm}},
\code{\link[data.table]{fread}},
\code{\link[dplyr]{tbl_df}}
}
\keyword{IO}
\keyword{manip}

