\name{impsampling}
\alias{impsampling}
\title{Importance sampling using a t proposal density}
\description{
 Implements importance sampling to compute the posterior mean of a function
using a multivariate t proposal density 
}
\usage{
impsampling(logf,tpar,h,n,data)
}
\arguments{
 \item{logf}{function that defines the logarithm of the density of interest}
  \item{tpar}{list of parameters of t proposal density including the mean m, scale matrix var, 
and degrees of freedom df} 
  \item{h}{function that defines h(theta)}
  \item{n}{number of simulated draws from proposal density}
  \item{data}{data and or parameters used in the function logf}
}
\value{
\item{est}{estimate at the posterior mean}
\item{se}{simulation standard error of estimate}
\item{theta}{matrix of simulated draws from proposal density}
\item{wt}{vector of importance sampling weights}
}
\author{Jim Albert}

\examples{
data(cancermortality)
start=c(-7,6)
fit=laplace(betabinexch,start,cancermortality)
tpar=list(m=fit$mode,var=2*fit$var,df=4)
myfunc=function(theta) return(theta[2])
theta=impsampling(betabinexch,tpar,myfunc,1000,cancermortality)
}

\keyword{models}
