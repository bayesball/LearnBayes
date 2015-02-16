\name{bayes.influence}
\alias{bayes.influence}
\title{Observation sensitivity analysis in beta-binomial model}
\description{
 Computes probability intervals for the log precision parameter K
in a beta-binomial model for all "leave one out" models using sampling
importance resampling 
}
\usage{
bayes.influence(theta,data)
}
\arguments{
 \item{theta}{matrix of simulated draws from the posterior of (logit eta, log K)}
  \item{data}{matrix with columns of counts and sample sizes}
}
\value{
\item{summary}{vector of 5th, 50th, 95th percentiles of log K for complete sample posterior}
\item{summary.obs}{matrix where the ith row contains the 5th, 50th, 95th percentiles
of log K for posterior when the ith observation is removed}
}
\author{Jim Albert}

\examples{
data(cancermortality)
start=array(c(-7,6),c(1,2))
fit=laplace(betabinexch,start,cancermortality)
tpar=list(m=fit$mode,var=2*fit$var,df=4)
theta=sir(betabinexch,tpar,1000,cancermortality)
intervals=bayes.influence(theta,cancermortality)
}

\keyword{models}
