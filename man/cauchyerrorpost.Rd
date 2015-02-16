\name{cauchyerrorpost}
\alias{cauchyerrorpost}
\title{Log posterior of median and log scale parameters for Cauchy sampling}
\description{
 Computes the log posterior density of (M,log S)  when
a sample is taken from a Cauchy density with location M and scale S and a uniform
prior distribution is taken on (M, log S)
}
\usage{
cauchyerrorpost(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values of M and log S}
  \item{data}{vector containing sample of observations}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
data=c(108, 51, 7, 43, 52, 54, 53, 49, 21, 48)
theta=c(40,1)
cauchyerrorpost(theta,data)
}

\keyword{models}
