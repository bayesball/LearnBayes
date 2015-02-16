\name{discrete.bayes}
\alias{discrete.bayes}
\alias{print.bayes}
\alias{plot.bayes}
\alias{summary.bayes}
\title{Posterior distribution with discrete priors}
\description{
Computes the posterior distribution for an arbitrary one parameter distribution
 for a discrete prior
distribution.
}
\usage{
discrete.bayes(df,prior,y,...)
}
\arguments{
  \item{df}{name of the function defining the sampling density}
  \item{prior}{vector defining the prior density; names of the vector
  define the parameter values and entries of the vector define 
  the prior probabilities}
  \item{y}{vector of data values}
  \item{...}{any further fixed parameter values used in the sampling density
  function}
}
\value{
  \item{prob}{vector of posterior probabilities}
  \item{pred}{scalar with prior predictive probability}
}

\author{Jim Albert}

\examples{
prior=c(.25,.25,.25,.25)
names(prior)=c(.2,.25,.3,.35)
y=5
n=10
discrete.bayes(dbinom,prior,y,size=n)
}

\keyword{models}
