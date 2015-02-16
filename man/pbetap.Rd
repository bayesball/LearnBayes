\name{pbetap}
\alias{pbetap}
\title{Predictive distribution for a binomial sample with a beta prior}
\description{
 Computes predictive distribution for number of successes of
 future binomial experiment
 with a beta prior distribution for the proportion.
}
\usage{
pbetap(ab, n, s)
}
\arguments{
  \item{ab}{vector of parameters of the beta prior}
  \item{n}{size of future binomial sample}
  \item{s}{vector of number of successes for future binomial experiment}
}

\value{
  vector of predictive probabilities for the values in the vector s
}

\author{Jim Albert}

\examples{
ab=c(3,12)
n=10
s=0:10
pbetap(ab,n,s)
}

\keyword{models}
