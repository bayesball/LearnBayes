\name{pdiscp}
\alias{pdiscp}
\title{Predictive distribution for a binomial sample with a discrete prior}
\description{
 Computes predictive distribution for number of successes of
 future binomial experiment
 with a discrete distribution for the proportion.
}
\usage{
pdiscp(p, probs, n, s)
}
\arguments{
  \item{p}{vector of proportion values}
  \item{probs}{vector of probabilities}
  \item{n}{size of future binomial sample}
  \item{s}{vector of number of successes for future binomial experiment}
}

\value{
  vector of predictive probabilities for the values in the vector s
}

\author{Jim Albert}

\examples{
p=c(.1,.2,.3,.4,.5,.6,.7,.8,.9)
prob=c(0.05,0.10,0.10,0.15,0.20,0.15,0.10,0.10,0.05)
n=10
s=0:10
pdiscp(p,prob,n,s)
}

\keyword{models}
