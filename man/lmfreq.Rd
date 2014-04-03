\name{lmfreq}
\alias{AIC.lmfreq}
\alias{extractAIC.lmfreq}
\alias{lmfreq}
\alias{logLik.lmfreq}
\alias{predict.lmfreq}
\alias{print.lmfreq}
\alias{print.summary.lmfreq}
\alias{summary.lmfreq}
\title{\code{lmfreq} is used to fit linear models with frequency tables}
\usage{
lmfreq(formula, data, freq = ~freq)

\method{logLik}{lmfreq}(object, ...)

\method{extractAIC}{lmfreq}(fit, scale = 0, k = 2, ...)

\method{AIC}{lmfreq}(object, ..., k = 2)

\method{summary}{lmfreq}(object, ...)

\method{print}{lmfreq}(x, ...)

\method{print}{summary.lmfreq}(x, digits = getOption("digits") - 3, ...)

\method{predict}{lmfreq}(object, ...)
}
\arguments{
  \item{formula}{an object of class \code{formula}}

  \item{data}{a data frame that must contain all variables
  present in \code{formula} and \code{freq}}

  \item{freq}{a one-sided, single term formula specifying
  frequency weights}

  \item{object}{a \code{lmfreq} object}

  \item{...}{See Details}

  \item{fit}{a \code{lmfreq} object}

  \item{scale}{not used}

  \item{k}{penalty parameter}

  \item{x}{a \code{lmfreq} object}

  \item{digits}{digits}
}
\value{
It returns an object of class \code{lmfreq}, very similar
to \code{\link[stats]{lm}}
}
\description{
To fit linear models with data grouped in frequency tables.
}
\details{
It computes the linear model of a frequency table. See
\code{\link[stats]{lm}} for further details.

Any variables in the formula are removed from the data set.
}
\examples{
l0 <- lm(Sepal.Length ~ Sepal.Width,iris)
summary(l0)

tt <- tablefreq(iris[,1:2])
lf <- lmfreq(Sepal.Length ~ Sepal.Width,tt)
summary(lf)

all.equal(coef(lf),coef(l0))
all.equal(AIC(lf),AIC(l0))

newdata <- data.frame(Sepal.Width=c(1,NA,7))
predict(lf, newdata)

if(require(MASS)){
   stepAIC(lf)
}
}
\seealso{
\code{\link{tablefreq}}
}

