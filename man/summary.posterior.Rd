\name{summary.posterior}
\alias{summary.posterior}
\title{Summary values from a posterior object}
\description{
summary.posterior will display univariate summaries from a  posterior density.
}
\usage{
\method{summary}{posterior}(object, ...)
}
\arguments{
  \item{object}{posterior object}
  \item{...}{any other arguments needed}
}

\author{Jim Albert}

\examples{
data <- c(2,4,3,6,1,0,4,3,10,2)
par <- c(0,1)
datapar <- list(data=data,par=par)
fit <- laplace(logpoissnormal, 1, datapar)
summary(fit)
}
\keyword{models}
