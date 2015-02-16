\name{sir}
\alias{sir}
\title{Sampling importance resampling}
\description{
 Implements sampling importance resampling for a multivariate
t proposal density. 
}
\usage{
sir(logf,tpar,n,data)
}
\arguments{
 \item{logf}{function defining logarithm of density of interest}
 \item{tpar}{list of parameters of multivariate t proposal density including
the mean m, the scale matrix var, and the degrees of freedom df}
 \item{n}{number of simulated draws from the posterior}
  \item{data}{data and parameters used in the function logf}
}
\value{
matrix of simulated draws from the posterior where each row corresponds to a single draw
}
\author{Jim Albert}

\examples{
data(cancermortality)
start=c(-7,6)
fit=laplace(betabinexch,start,cancermortality)
tpar=list(m=fit$mode,var=2*fit$var,df=4)
theta=sir(betabinexch,tpar,1000,cancermortality)
}

\keyword{models}
