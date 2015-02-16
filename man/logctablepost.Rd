\name{logctablepost}
\alias{logctablepost}
\title{Log posterior of difference and sum of logits in a 2x2 table}
\description{
 Computes the log posterior density for the difference and sum of logits
in a 2x2 contingency table for independent binomial samples and uniform
prior placed on the logits
}
\usage{
logctablepost(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values "difference of logits" and "sum of logits")}
  \item{data}{vector containing number of successes and failures for first sample, and then second sample}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
s1=6; f1=2; s2=3; f2=10
data=c(s1,f1,s2,f2)
theta=c(2,4)
logctablepost(theta,data)
}

\keyword{models}
