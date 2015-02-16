\name{bfexch}
\alias{bfexch}
\title{Logarithm of integral of Bayes factor for testing homogeneity of proportions}
\description{
Computes the logarithm of the integral of the Bayes factor for testing homogeneity of a set of proportions
}
\usage{
bfexch(theta,datapar)
}
\arguments{
  \item{theta}{value of the logit of the prior mean hyperparameter}
  \item{datapar}{list with components data, matrix with columns y (counts) and n (sample sizes), and K, prior
precision hyperparameter}
}

\value{
value of the logarithm of the integral
}

\author{Jim Albert}

\examples{
y=c(1,3,2,4,6,4,3)
n=c(10,10,10,10,10,10,10)
data=cbind(y,n)
K=20
datapar=list(data=data,K=K)
theta=1
bfexch(theta,datapar)
}

\keyword{models}
