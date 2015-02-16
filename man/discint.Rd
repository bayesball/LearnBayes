\name{discint}
\alias{discint}
\title{Highest probability interval for a discrete distribution}
\description{
Computes a highest probability interval for a discrete probability
distribution
}
\usage{
discint(dist, prob)
}
\arguments{
  \item{dist}{probability distribution written as a matrix where
  the first column contain the values and the second column 
  the probabilities}
  \item{prob}{probability content of interest}
}

\value{
  \item{prob}{exact probability content of interval}
  \item{set}{set of values of the probability interval}
}
\author{Jim Albert}

\examples{
x=0:10
probs=dbinom(x,size=10,prob=.3)
dist=cbind(x,probs)
pcontent=.8
discint(dist,pcontent)
}
\keyword{models}
