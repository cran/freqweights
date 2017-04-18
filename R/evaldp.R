## Emilio Torres Manzanera
## University of Oviedo
## Time-stamp: <2017-04-18 16:52 emilio on emilio-despacho>
## ============================================================

## \code{\link[dplyr]{manip}} --> manip


##' Eval a manip function using a string 
##'
##' Useful for programming with \code{\link[dplyr]{dplyr}}
##' @title Eval a manip function using a string 
##' @param .data a \code{\link[dplyr]{tbl}}
##' @param .fun.name any manip function
##' @param ... string arguments
##' @param .envir environment
##' @note It is possible that in the next release of \code{\link[dplyr]{dplyr}} these functionalities would appear. Then they will be removed from this package. 
##' @references
##' \url{https://gist.github.com/skranz/9681509}
## ## @seealso \code{\link[dplyr]{manip}}
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

## Borrowed from dplyr v0.5.0
partial_eval <- function(call, tbl = NULL, env = parent.frame()) {
  if (is.atomic(call)) return(call)

  if (inherits(call, "lazy_dots")) {
    lapply(call, function(l) partial_eval(l$expr, tbl, l$env))
  } else if (is.list(call)) {
    lapply(call, partial_eval, tbl = tbl, env = env)
  } else if (is.symbol(call)) {
    name <- as.character(call)
    if (!is.null(tbl) && name %in% tbl_vars(tbl)) {
      call
    } else if (exists(name, env)) {
      eval(call, env)
    } else {
      call
    }
  } else if (is.call(call)) {
    # Process call arguments recursively, unless user has manually called
    # remote/local
    name <- as.character(call[[1]])
    if (name == "local") {
      eval(call[[2]], env)
    } else if (name %in% c("$", "[[", "[")) {
      # Subsetting is always done locally
      eval(call, env)
    } else if (name == "remote") {
      call[[2]]
    } else {
      call[-1] <- lapply(call[-1], partial_eval, tbl = tbl, env = env)
      call
    }
  } else {
    stop("Unknown input type: ", class(call), call. = FALSE)
  }
}
