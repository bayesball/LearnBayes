\name{rtruncated}
\alias{rtruncated}
\title{Simulates from a truncated probability distribution}
\description{
 Simulates a sample from a truncated distribution where the functions for the cdf and inverse cdf are available.
} 
\usage{
rtruncated(n,lo,hi,pf,qf,...)
}
\arguments{
  \item{n}{size of simulated sample}
  \item{lo}{low truncation point}
  \item{hi}{high truncation point}
  \item{pf}{function containing cdf of untruncated distribution}
  \item{qf}{function containing inverse cdf of untruncated distribution}
  \item{...}{parameters used in the functions pf and qf}
}

\value{
vector of simulated draws from distribution}

\author{Jim Albert}

\examples{
# want a sample of 10 from normal(2, 1) distribution truncated below by 3
n=10
lo=3
hi=Inf
rtruncated(n,lo,hi,pnorm,qnorm,mean=2,sd=1)
# want a sample of 20 from beta(2, 5) distribution truncated to (.3, .8)
n=20
lo=0.3
hi=0.8
rtruncated(n,lo,hi,pbeta,qbeta,2,5)
}
\keyword{models}
