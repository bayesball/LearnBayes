\name{gibbs}
\alias{gibbs}
\title{Metropolis within Gibbs sampling algorithm of a posterior distribution}
\description{
 Implements a Metropolis-within-Gibbs sampling algorithm for an arbitrary real-valued
posterior density defined by the user
}
\usage{
gibbs(logpost,start,m,scale,...)
}
\arguments{
  \item{logpost}{function defining the log posterior density}
  \item{start}{array with a single row that gives the starting value of the parameter vector}
  \item{m}{the number of iterations of the chain}
  \item{scale}{vector of scale parameters for the random walk Metropolis steps}
  \item{...}{data that is used in the function logpost}
}

\value{
\item{par}{a matrix of simulated values where each row corresponds to a value of the vector parameter}
\item{accept}{vector of acceptance rates of the Metropolis steps of the algorithm}
}

\author{Jim Albert}

\examples{
data=c(6,2,3,10)
start=array(c(1,1),c(1,2))
m=1000
scale=c(2,2)
s=gibbs(logctablepost,start,m,scale,data)
}

\keyword{models}
