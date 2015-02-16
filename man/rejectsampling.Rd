\name{rejectsampling}
\alias{rejectsampling}
\title{Rejecting sampling using a t proposal density}
\description{
 Implements a rejection sampling algorithm for a probability density
using a multivariate t proposal density 
}
\usage{
rejectsampling(logf,tpar,dmax,n,data)
}
\arguments{
 \item{logf}{function that defines the logarithm of the density of interest}
  \item{tpar}{list of parameters of t proposal density including the mean m, scale matrix var, 
and degrees of freedom df} 
  \item{dmax}{logarithm of the rejection sampling constant}
  \item{n}{number of simulated draws from proposal density}
  \item{data}{data and or parameters used in the function logf}
}
\value{
matrix of simulated draws from density of interest
}
\author{Jim Albert}

\examples{
data(cancermortality)
start=c(-7,6)
fit=laplace(betabinexch,start,cancermortality)
tpar=list(m=fit$mode,var=2*fit$var,df=4)
theta=rejectsampling(betabinexch,tpar,-569.2813,1000,cancermortality)
}

\keyword{models}
