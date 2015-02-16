\name{reg.gprior.post}
\alias{reg.gprior.post}
\title{Computes the log posterior of a normal regression model with a g prior.}
\description{
 Computes the log posterior of (beta, log sigma) for a normal regression
model with a g prior with parameters beta0 and c0.
} 
\usage{
reg.gprior.post(theta, dataprior)
}
\arguments{
  \item{theta}{vector of components of beta and log sigma}
  \item{dataprior}{list with components data and prior; data is a list
with components y and X, prior is a list with components b0 and c0}
}
\value{
 value of the log posterior
}
\author{Jim Albert}

\examples{
data(puffin)
data=list(y=puffin$Nest, X=cbind(1,puffin$Distance))
prior=list(b0=c(0,0), c0=10)
reg.gprior.post(c(20,-.5,1),list(data=data,prior=prior))
}

\keyword{models}
