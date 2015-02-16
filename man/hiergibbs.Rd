\name{hiergibbs}
\alias{hiergibbs}
\title{Gibbs sampling for a hierarchical regression model}
\description{
 Implements Gibbs sampling for estimating a two-way table of means
under a hierarchical regression model.
}
\usage{
hiergibbs(data,m)
}
\arguments{
  \item{data}{data matrix with columns observed sample means, sample sizes, and values of two covariates}
  \item{m}{number of cycles of Gibbs sampling}
}

\value{
  \item{beta}{matrix of simulated values of regression vector}
  \item{mu}{matrix of simulated values of cell means}
  \item{var}{vector of simulated values of second-stage prior variance}
}

\author{Jim Albert}

\examples{
data(iowagpa)
m=1000
s=hiergibbs(iowagpa,m)
}

\keyword{models}
