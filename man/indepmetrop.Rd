\name{indepmetrop}
\alias{indepmetrop}
\title{Independence Metropolis independence chain of a posterior distribution}
\description{
 Simulates iterates of an independence Metropolis chain with a normal proposal density for an arbitrary real-valued
posterior density defined by the user}
\usage{
indepmetrop(logpost,proposal,start,m,stuff)
}
\arguments{
 \item{logpost}{function defining the log posterior density}
  \item{proposal}{a list containing mu, an estimated mean and var, an estimated variance-covariance matrix, of the normal proposal density} 
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
proposal=list(mu=array(c(2.3,-.1),c(2,1)),var=diag(c(1,1)))
start=array(c(0,0),c(1,2))
m=1000
fit=indepmetrop(logctablepost,proposal,start,m,data)
}

\keyword{models}
