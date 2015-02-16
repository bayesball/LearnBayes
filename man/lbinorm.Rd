\name{lbinorm}
\alias{lbinorm}
\title{Logarithm of bivariate normal density}
\description{
Computes the logarithm of a bivariate normal density
}
\usage{
lbinorm(xy,par)
}
\arguments{
  \item{xy}{vector of values of two variables x and y}
  \item{par}{list with components m, a vector of means, and v, a variance-covariance matrix}
}

\value{
value of the kernel of the log density
}

\author{Jim Albert}

\examples{
mean=c(0,0)
varcov=diag(c(1,1))
value=c(1,1)
param=list(m=mean,v=varcov)
lbinorm(value,param)
}

\keyword{models}
