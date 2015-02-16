\name{triplot}
\alias{triplot}
\title{Plot of prior, likelihood and posterior for a proportion}
\description{
For a proportion problem with a beta prior, plots the prior, likelihood and posterior
on one graph.
}
\usage{
triplot(prior,data,where="topright")
}
\arguments{
  \item{prior}{vector of parameters for beta prior}
  \item{data}{vector consisting of number of successes and number of
  failures}
  \item{where}{the location of the legend for the plot}
}

\author{Jim Albert}

\examples{
prior=c(3,10)  # proportion has a beta(3, 10) prior
data=c(10,6)   # observe 10 successes and 6 failures
triplot(prior,data)
}

\keyword{models}
