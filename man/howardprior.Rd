\name{howardprior}
\alias{howardprior}
\title{Logarithm of Howard's dependent prior for two proportions}
\description{
Computes the logarithm of a dependent prior on two proportions
proposed by Howard in a Statistical Science paper in 1998.
}
\usage{
howardprior(xy,par)
}
\arguments{
  \item{xy}{vector of proportions p1 and p2}
  \item{par}{vector containing parameter values alpha, beta, gamma, delta, sigma}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
param=c(1,1,1,1,2)
p=c(.1,.5)
howardprior(p,param)
}

\keyword{models}
