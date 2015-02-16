\name{histprior}
\alias{histprior}
\title{Density function of a histogram distribution}
\description{
Computes the density of a probability distribution defined on a set
of equal-width intervals
}
\usage{
histprior(p,midpts,prob)
}
\arguments{
  \item{p}{vector of values for which density is to be computed}
  \item{midpts}{vector of midpoints of the intervals}
  \item{prob}{vector of probabilities of the intervals}
}

\value{
vector of values of the probability density
}

\author{Jim Albert}

\examples{
midpts=c(.1,.3,.5,.7,.9)
prob=c(.2,.2,.4,.1,.1)
p=seq(.01,.99,by=.01)
plot(p,histprior(p,midpts,prob),type="l")
}

\keyword{models}
