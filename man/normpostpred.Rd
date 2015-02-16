\name{normpostpred}
\alias{normpostpred}
\title{Posterior predictive simulation from Bayesian normal sampling model}
\description{
 Given simulated draws from the posterior from a normal sampling model, outputs
simulated draws from the posterior predictive distribution of a statistic of interest.
}
\usage{
normpostpred(parameters,sample.size,f=min)
}
\arguments{
  \item{parameters}{list of simulated draws from the posterior where mu contains the normal mean
and sigma2 contains the normal variance}
  \item{sample.size}{size of sample of future sample}
  \item{f}{function defining the statistic}
}

\value{
simulated sample of the posterior predictive distribution of the statistic}
\author{Jim Albert}

\examples{
# finds posterior predictive distribution of the min statistic of a future sample of size 15
data(darwin)
s=normpostsim(darwin$difference)
sample.size=15
sim.stats=normpostpred(s,sample.size,min)
}

\keyword{models}
