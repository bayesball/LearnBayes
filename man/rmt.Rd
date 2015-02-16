\name{rmt}
\alias{rmt}
\title{Random number generation for multivariate t}
\description{
 Simulates from a multivariate t distribution
}
\usage{
rmt(n = 1, mean = rep(0, d), S, df = Inf)
}
\arguments{
  \item{n}{number of random numbers to be generated}
  \item{mean}{numeric vector giving the location parameter of the distribution}
  \item{S}{a positive definite matrix representing the scale matrix of the distribution}
  \item{df}{degrees of freedom}
}
\value{
matrix of n rows of random vectors
}

\author{Jim Albert}

\examples{
mu <- c(1,12,2)
Sigma <- matrix(c(1,2,0,2,5,0.5,0,0.5,3), 3, 3)
df <- 4
x <- rmt(10, mu, Sigma, df)
}

\keyword{models}

