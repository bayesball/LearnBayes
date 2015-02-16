\name{predplot}
\alias{predplot}
\title{Plot of predictive distribution for binomial sampling with a beta prior}
\description{
For a proportion problem with a beta prior, plots the prior predictive distribution
of the number of successes in n trials and displays the observed number of successes.
}
\usage{
predplot(prior,n,yobs)
}
\arguments{
  \item{prior}{vector of parameters for beta prior}
  \item{n}{sample size}
  \item{yobs}{observed number of successes}
}

\author{Jim Albert}

\examples{
prior=c(3,10)  # proportion has a beta(3, 10) prior
n=20   # sample size
yobs=10  # observed number of successes
predplot(prior,n,yobs)
}

\keyword{models}
