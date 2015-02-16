\name{betabinexch0}
\alias{betabinexch0}
\title{Log posterior of mean and precision for Binomial/beta exchangeable model}
\description{
 Computes the log posterior density of mean and precision for a Binomial/beta exchangeable model
}
\usage{
betabinexch0(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values of eta and K}
  \item{data}{a matrix with columns y (counts) and n (sample sizes)}
}

\value{
value of the log posterior}

\author{Jim Albert}

\examples{
n=c(20,20,20,20,20)
y=c(1,4,3,6,10)
data=cbind(y,n)
theta=c(.1,10)
betabinexch0(theta,data)
}

\keyword{models}
