\name{ctable}
\alias{ctable}
\title{Bayes factor against independence using uniform priors}
\description{
Computes a Bayes factor against independence for a two-way contingency table assuming
uniform prior distributions}
\usage{
ctable(y,a)
}
\arguments{
  \item{y}{matrix of counts}
  \item{a}{matrix of prior hyperparameters}
}

\value{
value of the Bayes factor against independence
}

\author{Jim Albert}

\examples{
y=matrix(c(10,4,6,3,6,10),c(2,3))
a=matrix(rep(1,6),c(2,3))
ctable(y,a)
}

\keyword{models}
