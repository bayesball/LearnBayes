\name{ordergibbs}
\alias{ordergibbs}
\title{Gibbs sampling for a hierarchical regression model}
\description{
 Implements Gibbs sampling for estimating a two-way table of means
under a order restriction.
}
\usage{
ordergibbs(data,m)
}
\arguments{
  \item{data}{data matrix with first two columns observed sample means and sample sizes}
  \item{m}{number of cycles of Gibbs sampling}
}

\value{
  matrix of simulated draws of the normal means where each row represents one simulated draw
}

\author{Jim Albert}

\examples{
data(iowagpa)
m=1000
s=ordergibbs(iowagpa,m)
}

\keyword{models}
