\name{biglmfreq}
\alias{biglmfreq}
\alias{coef.biglmfreq}
\alias{predict.biglmfreq}
\alias{print.biglmfreq}
\alias{update.biglmfreq}
\title{Estimates the coefficients of a linear model}
\usage{
biglmfreq(formula, data, freq = ~freq)

\method{coef}{biglmfreq}(object, ...)

\method{predict}{biglmfreq}(object, ...)

\method{print}{biglmfreq}(x, ...)

\method{update}{biglmfreq}(object, ...)
}
\arguments{
  \item{formula}{a model formula}

  \item{data}{data frame that must contain all variables in
  \code{formula} and \code{freq}}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights}

  \item{object}{a \code{biglmfreq} object}

  \item{...}{See Details}

  \item{x}{a \code{biglmfreq} object}
}
\value{
A \code{biglmfreq} object.
}
\description{
Estimates the coefficients of a linear model following the
guidelines of \code{\link[biglm]{biglm}}
}
\details{
Any variables in the formula are removed from the data set.

It only computes the coefficients of the linear model.

\code{\dots} should be a data frame when \code{predict}.
See Examples

\code{\dots} should be a data frame when \code{update}. See
Examples
}
\examples{
ml <- lm(Sepal.Length ~ Sepal.Width, iris)
tt <- tablefreq(iris[,c("Sepal.Length","Sepal.Width")])
mt <- biglmfreq(Sepal.Length ~ Sepal.Width, tt)
all.equal(coef(mt), coef(ml))
coef(mt)

chunk1 <- tt[1:30,]
mf1 <- biglmfreq(Sepal.Length ~ Sepal.Width, chunk1)

chunk2 <- tt[-c(1:30),]
mf2 <- update(mf1, chunk2)

all.equal(coef(mf2), coef(ml))

predict(mf2, iris)
}
\seealso{
\code{\link[biglm]{biglm}}, \code{\link{make.readchunk}}
}

