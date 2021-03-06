% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biglmfreq.R
\name{biglmfreq}
\alias{biglmfreq}
\alias{coef.biglmfreq}
\alias{predict.biglmfreq}
\alias{update.biglmfreq}
\alias{coef.biglmfreq}
\alias{predict.biglmfreq}
\alias{print.biglmfreq}
\alias{update.biglmfreq}
\title{Estimates the coefficients of a linear model}
\usage{
biglmfreq(formula, data, freq = NULL)

\method{coef}{biglmfreq}(object, ...)

\method{predict}{biglmfreq}(object, ...)

\method{print}{biglmfreq}(x, ...)

\method{update}{biglmfreq}(object, ...)
}
\arguments{
\item{formula}{a model formula}

\item{data}{data frame that must contain all variables in \code{formula}
and \code{freq}}

\item{freq}{a string of the variable specifying frequency weights}

\item{object}{a \code{biglmfreq} object}

\item{...}{See Details}

\item{x}{a \code{biglmfreq} object}
}
\value{
A \code{biglmfreq} object.
}
\description{
Estimates the coefficients of a linear model following the guidelines of
\code{\link[biglm]{biglm}}
}
\details{
Any variables in the formula are removed from the data set.

It only computes the coefficients of the linear model.

\code{\dots} should be a data frame when \code{predict}. See Examples

\code{\dots} should be a data frame when \code{update}. See Examples
}
\examples{
mt <- biglmfreq(Sepal.Length ~ Sepal.Width, iris)
coef(mt)

chunk1 <- iris[1:30,]
chunk2 <- iris[-c(1:30),]
mf1 <- biglmfreq(Sepal.Length ~ Sepal.Width, chunk1)
mf2 <- update(mf1, chunk2)

predict(mf2, iris)
}
\seealso{
\code{\link[biglm]{biglm}},  \code{\link{make.readchunk}}
}
