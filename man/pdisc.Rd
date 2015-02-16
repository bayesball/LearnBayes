\name{pdisc}
\alias{pdisc}
\title{Posterior distribution for a proportion with discrete priors}
\description{
Computes the posterior distribution for a proportion for a discrete prior
distribution.
}
\usage{
pdisc(p, prior, data)
}
\arguments{
  \item{p}{vector of proportion values}
  \item{prior}{vector of prior probabilities}
  \item{data}{vector consisting of number of successes and number of
  failures}
}
\value{
  vector of posterior probabilities
}

\author{Jim Albert}

\examples{
p=c(.2,.25,.3,.35)
prior=c(.25,.25,.25,.25)
data=c(5,10)
pdisc(p,prior,data)
}

\keyword{models}
