\name{groupeddatapost}
\alias{groupeddatapost}
\title{Log posterior of normal parameters when data is in grouped form}
\description{
 Computes the log posterior density of (M,log S) for normal sampling where the data is
observed in grouped form
}
\usage{
groupeddatapost(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values M and log S}
  \item{data}{list with components int.lo, a vector of left endpoints,
int.hi, a vector of right endpoints, and f, a vector of bin frequencies}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
int.lo=c(-Inf,10,15,20,25)
int.hi=c(10,15,20,25,Inf)
f=c(2,5,8,4,2)
data=list(int.lo=int.lo,int.hi=int.hi,f=f)
theta=c(20,1)
groupeddatapost(theta,data)
}

\keyword{models}
