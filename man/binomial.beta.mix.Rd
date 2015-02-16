\name{binomial.beta.mix}
\alias{binomial.beta.mix}
\title{Computes the posterior for binomial sampling and a mixture of betas prior}
\description{
 Computes the parameters and mixing probabilities for a binomial sampling problem
where the prior is a discrete mixture of beta densities.
} 
\usage{
binomial.beta.mix(probs,betapar,data)
}
\arguments{
  \item{probs}{vector of probabilities of the beta components of the prior}
  \item{betapar}{matrix where each row contains the shape parameters for a beta component of the prior}
  \item{data}{vector of number of successes and number of failures}
}

\value{
\item{probs}{vector of  probabilities of the beta components of the posterior}
\item{betapar}{matrix where each row contains the shape parameters for a beta component of the posterior}
}
\author{Jim Albert}

\examples{
probs=c(.5, .5)
beta.par1=c(15,5)
beta.par2=c(10,10)
betapar=rbind(beta.par1,beta.par2)
data=c(20,15)
binomial.beta.mix(probs,betapar,data)
}

\keyword{models}
