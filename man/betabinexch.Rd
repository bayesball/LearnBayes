\name{betabinexch}
\alias{betabinexch}
\title{Log posterior of logit mean and log precision for Binomial/beta exchangeable model}
\description{
 Computes the log posterior density of logit mean and log precision for a Binomial/beta exchangeable model
}
\usage{
betabinexch(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values of logit eta and log K}
  \item{data}{a matrix with columns y (counts) and n (sample sizes)}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
n=c(20,20,20,20,20)
y=c(1,4,3,6,10)
data=cbind(y,n)
theta=c(-1,0)
betabinexch(theta,data)
}

\keyword{models}
