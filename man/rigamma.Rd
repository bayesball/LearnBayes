\name{rigamma}
\alias{rigamma}
\title{Random number generation for inverse gamma distribution}
\description{
 Simulates from a inverse gamma (a, b) distribution with density 
proportional to $y^(-a-1) exp(-b/y)$
}
\usage{
rigamma(n, a, b)
}
\arguments{
  \item{n}{number of random numbers to be generated}
  \item{a}{inverse gamma shape parameter}
  \item{b}{inverse gamma rate parameter}
}
\value{
vector of n simulated draws
}

\author{Jim Albert}

\examples{
a=10
b=5
n=20
rigamma(n,a,b)
}

\keyword{models}

