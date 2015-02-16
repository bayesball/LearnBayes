\name{simcontour}
\alias{simcontour}
\title{Simulated draws from a bivariate density function on a grid}
\description{
 For a general two parameter density defined on a grid, simulates a random sample.
}
\usage{
simcontour(logf,limits,data,m)
}
\arguments{
  \item{logf}{function that defines the logarithm of the  density}
  \item{limits}{limits (xlo, xhi, ylo, yhi) that cover the joint probability density}
  \item{data}{vector or list of parameters associated with the function logpost}
  \item{m}{size of simulated sample}
}

\value{
\item{x}{vector of simulated draws of the first parameter}
\item{y}{vector of simulated draws of the second parameter}
}

\author{Jim Albert}

\examples{
m=array(c(0,0),c(2,1))
v=array(c(1,.6,.6,1),c(2,2))
normpar=list(m=m,v=v)
s=simcontour(lbinorm,c(-4,4,-4,4),normpar,1000)
plot(s$x,s$y)
}

\keyword{models}
