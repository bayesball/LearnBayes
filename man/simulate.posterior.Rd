\name{simulate.posterior}
\alias{simulate.posterior}
\title{Simulate values from a posterior object}
\description{
simulate.posterior will simulate draws from a  posterior density.
}
\usage{
\method{simulate}{posterior}(object, nsim=1000, seed=NULL, ...)
}
\arguments{
  \item{object}{posterior object}
  \item{nsim}{number of simulate iterates}
  \item{seed}{starting seed of random number generator}
  \item{...}{optional arguments; use exact=TRUE to simulate from the exact posterior density and the scale argument indicates the scale value for the Metropolis random walk algorithm}
}
\value{
\item{sample}{matrix of simulated draws of posterior}
\item{arate}{acceptance rate if the Metropolis random walk algorithm is used}
}
\author{Jim Albert}

\examples{
data <- c(2,4,3,6,1,0,4,3,10,2)
par <- c(0,1)
datapar <- list(data=data,par=par)
fit <- laplace(logpoissnormal, 1, datapar)
simulate(fit, exact=TRUE, scale=4)
}
\keyword{models}
