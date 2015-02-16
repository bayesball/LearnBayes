\name{beta.select}
\alias{beta.select}
\title{Selection of Beta Prior Given Knowledge of Two Quantiles}
\description{
 Finds the shape parameters of a beta density that matches knowledge of
two quantiles of the distribution.
}
\usage{
beta.select(quantile1, quantile2)
}
\arguments{
  \item{quantile1}{list with components p, the value of the first probability,
  and x, the value of the first quantile}
  \item{quantile2}{list with components p, the value of the second probability,
  and x, the value of the second quantile}
}

\value{
vector of shape parameters of the matching beta distribution
}

\author{Jim Albert}

\examples{
# person believes the median of the prior is 0.25 
# and the 90th percentile of the prior is 0.45
quantile1=list(p=.5,x=0.25)
quantile2=list(p=.9,x=0.45)
beta.select(quantile1,quantile2)
}

\keyword{models}
