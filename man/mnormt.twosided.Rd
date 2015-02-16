\name{mnormt.twosided}
\alias{mnormt.twosided}
\title{Bayesian test of a two-sided hypothesis about a normal mean}
\description{
 Bayesian test that a normal mean is equal to a specified
 value using a normal prior}
\usage{
mnormt.twosided(m0, prob, t, data)
}
\arguments{
  \item{m0}{value of the mean to be tested }
  \item{prob}{prior probability of the hypothesis}
  \item{t}{vector of values of the prior standard
  deviation under the alternative hypothesis}
  \item{data}{vector containing the sample mean,
  the sample size, and the known value of the
  population standard deviation}
}

\value{
  \item{bf}{vector of values of the Bayes factor
  in support of the null hypothesis}
  \item{post}{vector of posterior probabilities
  of the null hypothesis}
}

\author{Jim Albert}

\examples{
m0=170
prob=.5
tau=c(.5,1,2,4,8)
samplesize=10
samplemean=176
popsd=3
data=c(samplemean,samplesize,popsd)
mnormt.twosided(m0,prob,tau,data)
}

\keyword{models}
