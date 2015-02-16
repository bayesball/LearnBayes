\name{laplace}
\alias{laplace}
\title{Summarization of a posterior density by the Laplace method}
\description{
 For a general posterior density, computes the posterior mode, 
the associated variance-covariance matrix, and an estimate at the
logarithm at the normalizing constant.
}
\usage{
laplace(logpost,mode,stuff)
}
\arguments{
  \item{logpost}{function that defines the logarithm of the posterior density}
  \item{mode}{vector that is a guess at the posterior mode}
  \item{stuff}{data and parameters associated with the function logpost}
}

\value{
\item{mode}{current estimate at the posterior mode}
\item{var}{current estimate at the associated variance-covariance matrix}
\item{int}{estimate at the logarithm of the normalizing constant}
\item{converge}{indication (TRUE or FALSE) if the algorithm converged}
\item{stuff}{data and parameters associated with the function logpost}
\item{logpost}{function that defines the log posterior}
}

\author{Jim Albert}

\examples{
logpost=function(theta,data)
{
s=5
sum(-log(1+(data-theta)^2/s^2))
}
data=c(10,12,14,13,12,15)
start=10
laplace(logpost,start,data)
}

\keyword{models}
