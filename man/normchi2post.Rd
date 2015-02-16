\name{normchi2post}
\alias{normchi2post}
\title{Log posterior density for mean and variance for normal sampling}
\description{
 Computes the log of the posterior density of a mean M and a variance S2 when a sample is taken from a normal density and a standard noninformative prior is used.
}
\usage{
normchi2post(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values M and S2}
  \item{data}{vector containing the sample observations}
}
\value{
value of the log posterior
}
\author{Jim Albert}

\examples{
parameter=c(25,5)
data=c(20, 32, 21, 43, 33, 21, 32)
normchi2post(parameter,data)
}

\keyword{models}
