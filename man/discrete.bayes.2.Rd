\name{discrete.bayes.2}
\alias{discrete.bayes.2}
\alias{plot.bayes2}
\title{Posterior distribution of two parameters with discrete priors}
\description{
Computes the posterior distribution for an arbitrary two parameter distribution
 for a discrete prior
distribution.
}
\usage{
discrete.bayes.2(df,prior,y=NULL,...)
}
\arguments{
  \item{df}{name of the function defining the sampling density of
two parameters}
  \item{prior}{matrix defining the prior density; the row names and column names
of the matrix define respectively the values of parameter 1 and values of
parameter 2 and the entries of the matrix give the prior probabilities}
  \item{y}{y is a matrix of data values, where each row corresponds to a 
single observation}
  \item{...}{any further fixed parameter values 
used in the sampling density function}
}

\value{
  \item{prob}{matrix of posterior probabilities}
  \item{pred}{scalar with prior predictive probability}
}

\author{Jim Albert}

\examples{
p1 = seq(0.1, 0.9, length = 9)
p2 = p1
prior = matrix(1/81, 9, 9)
dimnames(prior)[[1]] = p1
dimnames(prior)[[2]] = p2
discrete.bayes.2(twoproplike,prior)
}

\keyword{models}
