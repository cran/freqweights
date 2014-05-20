## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2014-04-24 19:43 emilio on emilio-Satellite-P100>
## ============================================================



##' Eval a \code{\link[dplyr]{manip}} function using a string 
##'
##' Useful for programming with \code{\link[dplyr]{manip}}
##' @title Eval a \code{\link[dplyr]{manip}} function using a string 
##' @param .data a \code{\link[dplyr]{tbl}}
##' @param .fun.name any \code{\link[dplyr]{manip}} function
##' @param ... string arguments
##' @param .envir environment
##' @note It is possible that in the next release of \code{\link[dplyr]{dplyr}} these functionalities would appear. Then they will be removed from this package. 
##' @references
##' \url{https://gist.github.com/skranz/9681509}
##' @seealso \code{\link[dplyr]{manip}}
##' @import dplyr
##' 
##' @examples
##' library(dplyr)
##' iris %>% evaldp(arrange,"Sepal.Length") %>%
##'          evaldp(filter, "Sepal.Length > 5, Species=='virginica'")
##' @export
evaldp <- function(.data, .fun.name, ..., .envir= parent.frame()) {
  ## https://gist.github.com/skranz/9681509
  args <- list(...)
  args <- partial_eval(args, env=.envir)
  args <- unlist(args)
  if(is.function(.fun.name)) .fun.name <- as.character(substitute(.fun.name))
  code <- paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  eval(parse(text=code,srcfile=NULL))
}
