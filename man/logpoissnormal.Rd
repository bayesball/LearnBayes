\name{logpoissnormal}
\alias{logpoissnormal}
\title{Log  posterior with Poisson sampling and normal prior}
\description{
Computes the logarithm of the posterior density of a Poisson log mean
with a normal prior
}
\usage{
logpoissnormal(theta,datapar)
}
\arguments{
  \item{theta}{vector of values of the log mean parameter}
  \item{datapar}{list with components data, vector of observations, and par, vector of
parameters of the normal prior}
}

\value{
vector of values of the log posterior for all values in theta
}

\author{Jim Albert}

\examples{
data=c(2,4,3,6,1,0,4,3,10,2)
par=c(0,1)
datapar=list(data=data,par=par)
theta=c(-1,0,1,2)
logpoissnormal(theta,datapar)
}

\keyword{models}
