\name{transplantpost}
\alias{transplantpost}
\title{Log posterior of a Pareto model for survival data}
\description{
 Computes the log posterior density of (log tau, log lambda, log p)  for a Pareto
model for survival data
}
\usage{
transplantpost(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values of log tau, log lambda, and log p}
  \item{data}{data matrix with columns survival time, transplant indicator, time to transplant, and
censoring indicator}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
data(stanfordheart)
theta=c(0,3,-1)
transplantpost(theta,stanfordheart)
}

\keyword{models}
