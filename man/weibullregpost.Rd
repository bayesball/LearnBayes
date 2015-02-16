\name{weibullregpost}
\alias{weibullregpost}
\title{Log posterior of a Weibull proportional odds model for survival data}
\description{
 Computes the log posterior density of (log sigma, mu, beta) for a Weibull
proportional odds regression model
}
\usage{
weibullregpost(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values log sigma, mu, and beta}
  \item{data}{data matrix with columns survival time, censoring variable, and covariate matrix}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
data(chemotherapy)
attach(chemotherapy)
d=cbind(time,status,treat-1,age)
theta=c(-.6,11,.6,0)
weibullregpost(theta,d)
}

\keyword{models}
