\name{rmnorm}
\alias{rmnorm}
\title{Random number generation for multivariate normal}
\description{
 Simulates from a multivariate normal distribution
}
\usage{
rmnorm(n = 1, mean = rep(0, d), varcov)
}
\arguments{
  \item{n}{number of random numbers to be generated}
  \item{mean}{numeric vector giving the mean of the distribution}
  \item{varcov}{a positive definite matrix representing the variance-covariance matrix of the distribution}
}
\value{
matrix of n rows of random vectors
}

\author{Jim Albert}

\examples{
mu <- c(1,12,2)
Sigma <- matrix(c(1,2,0,2,5,0.5,0,0.5,3), 3, 3)
x <- rmnorm(10, mu, Sigma)
}

\keyword{models}

