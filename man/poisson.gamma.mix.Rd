\name{poisson.gamma.mix}
\alias{poisson.gamma.mix}
\title{Computes the posterior for Poisson sampling and a mixture of gammas prior}
\description{
 Computes the parameters and mixing probabilities for a Poisson sampling problem
where the prior is a discrete mixture of gamma densities.
} 
\usage{
poisson.gamma.mix(probs,gammapar,data)
}
\arguments{
  \item{probs}{vector of probabilities of the gamma components of the prior}
  \item{gammapar}{matrix where each row contains the shape and rate parameters for a gamma component of the prior}
  \item{data}{list with components y, vector of counts, and t, vector of time intervals}
}

\value{
\item{probs}{vector of  probabilities of the gamma components of the posterior}
\item{gammapar}{matrix where each row contains the shape and rate parameters for a gamma component of the posterior}
}
\author{Jim Albert}

\examples{
probs=c(.5, .5)
gamma.par1=c(1,1)
gamma.par2=c(10,2)
gammapar=rbind(gamma.par1,gamma.par2)
y=c(1,3,2,4,10); t=c(1,1,1,1,1)
data=list(y=y,t=t)
poisson.gamma.mix(probs,gammapar,data)}

\keyword{models}
