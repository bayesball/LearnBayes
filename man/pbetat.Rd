\name{pbetat}
\alias{pbetat}
\title{Bayesian test of a proportion}
\description{
 Bayesian test that a proportion is equal to a specified
 value using a beta prior}
\usage{
pbetat(p0,prob,ab,data)
}
\arguments{
  \item{p0}{value of the proportion to be tested }
  \item{prob}{prior probability of the hypothesis}
  \item{ab}{vector of parameter values of the beta prior under the alternative hypothesis}
  \item{data}{vector containing the number of successes and number of failures}
}

\value{
  \item{bf}{the Bayes factor
  in support of the null hypothesis}
  \item{post}{the posterior probability
  of the null hypothesis}
}

\author{Jim Albert}

\examples{
p0=.5
prob=.5
ab=c(10,10)
data=c(5,15)
pbetat(p0,prob,ab,data)
}

\keyword{models}
