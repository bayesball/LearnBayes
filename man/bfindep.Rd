\name{bfindep}
\alias{bfindep}
\title{Bayes factor against independence assuming alternatives close to independence}
\description{
Computes a Bayes factor against independence for a two-way contingency table assuming
a "close to independence" alternative model}
\usage{
bfindep(y,K,m)
}
\arguments{
  \item{y}{matrix of counts}
  \item{K}{Dirichlet precision hyperparameter}
  \item{m}{number of simulations}
}

\value{
\item{bf}{value of the Bayes factor against hypothesis of independence}
\item{nse}{estimate of the simulation standard error of the computed Bayes factor}
}

\author{Jim Albert}

\examples{
y=matrix(c(10,4,6,3,6,10),c(2,3))
K=20
m=1000
bfindep(y,K,m)
}

\keyword{models}
