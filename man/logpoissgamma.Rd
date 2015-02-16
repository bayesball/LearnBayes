\name{logpoissgamma}
\alias{logpoissgamma}
\title{Log posterior with Poisson sampling and gamma prior}
\description{
Computes the logarithm of the posterior density of a Poisson log mean
with a gamma prior
}
\usage{
logpoissgamma(theta,datapar)
}
\arguments{
  \item{theta}{vector of values of the log mean parameter}
  \item{datapar}{list with components data, vector of observations, and par, vector of
parameters of the gamma prior}
}

\value{
vector of values of the log posterior for all values in theta
}

\author{Jim Albert}

\examples{
data=c(2,4,3,6,1,0,4,3,10,2)
par=c(1,1)
datapar=list(data=data,par=par)
theta=c(-1,0,1,2)
logpoissgamma(theta,datapar)
}

\keyword{models}
