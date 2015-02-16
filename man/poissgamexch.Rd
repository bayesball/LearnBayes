\name{poissgamexch}
\alias{poissgamexch}
\title{Log posterior of Poisson/gamma exchangeable model}
\description{
 Computes the log posterior density of log alpha and log mu for a Poisson/gamma exchangeable model
}
\usage{
poissgamexch(theta,datapar)
}
\arguments{
  \item{theta}{vector of parameter values of log alpha and log mu}
  \item{datapar}{list with components data, a matrix with columns e and y, and z0, prior hyperparameter}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
e=c(532,584,672,722,904)
y=c(0,0,2,1,1)
data=cbind(e,y)
theta=c(-4,0)
z0=.5
datapar=list(data=data,z0=z0)
poissgamexch(theta,datapar)
}

\keyword{models}
