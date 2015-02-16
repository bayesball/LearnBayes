\name{normnormexch}
\alias{normnormexch}
\title{Log posterior of mean and log standard deviation for Normal/Normal exchangeable model}
\description{
 Computes the log posterior density of mean and log standard deviation for a Normal/Normal exchangeable model where (mean, log sd) is given a uniform prior.
}
\usage{
normnormexch(theta,data)
}
\arguments{
  \item{theta}{vector of parameter values of mu and log tau}
  \item{data}{a matrix with columns y (observations) and v (sampling variances)}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
s.var <- c(0.05, 0.05, 0.05, 0.05, 0.05)
y.means <- c(1, 4, 3, 6,10)
data=cbind(y.means, s.var)
theta=c(-1, 0)
normnormexch(theta,data)
}

\keyword{models}
