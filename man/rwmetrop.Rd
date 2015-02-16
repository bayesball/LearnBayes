\name{rwmetrop}
\alias{rwmetrop}
\title{Random walk Metropolis algorithm of a posterior distribution}
\description{
 Simulates iterates of a random walk Metropolis chain for an arbitrary  real-valued
posterior density defined by the user
}
\usage{
rwmetrop(logpost,proposal,start,m,stuff)
}
\arguments{
  \item{logpost}{function defining the log posterior density}
  \item{proposal}{a list containing var, an estimated variance-covariance matrix, and scale, the Metropolis scale factor} 
  \item{start}{vector containing the starting value of the parameter}
  \item{m}{the number of iterations of the chain}
  \item{stuff}{data and prior info that is used in the function logpost}
}

\value{
\item{par}{a matrix of simulated values where each row corresponds to a value of the vector parameter}
\item{accept}{the acceptance rate of the algorithm}
}

\author{Jim Albert}

\examples{
data=c(6,2,3,10)
varcov=diag(c(1,1))
proposal=list(var=varcov,scale=2)
start=array(c(1,1),c(1,2))
m=1000
s=rwmetrop(logctablepost,proposal,start,m,data)
}

\keyword{models}
