\name{dmt}
\alias{dmt}
\title{Probability density function for multivariate t}
\description{
 Computes the density of a multivariate t distribution
}
\usage{
dmt(x, mean = rep(0, d), S, df = Inf, log=FALSE)
}
\arguments{
  \item{x}{vector of length d or matrix with d columns, giving the coordinates of points where density is to evaluated}
  \item{mean}{numeric vector giving the location parameter of the distribution}
  \item{S}{a positive definite matrix representing the scale matrix of the distribution}
  \item{df}{degrees of freedom}
  \item{log}{a logical value; if TRUE, the logarithm of the density is to be computed}
}
\value{
vector of density values
}

\author{Jim Albert}

\examples{
mu <- c(1,12,2)
Sigma <- matrix(c(1,2,0,2,5,0.5,0,0.5,3), 3, 3)
df <- 4
x <- c(2,14,0)
f <- dmt(x, mu, Sigma, df)
}

\keyword{models}

