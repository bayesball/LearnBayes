\name{plot.posterior}
\alias{plot.posterior}
\title{Plot of a posterior object}
\description{
plot.posterior constructs a plot of the posterior density of one or two variables.  For 2-parameter problems, a contour plot is produced.
}
\usage{
\method{plot}{posterior}(x, ...)
}
\arguments{
  \item{x}{posterior object}
  \item{...}{optional arguments; use exact=TRUE to plot the exact posterior density and the scale argument indicates the range of the axes of the plot}
}

\author{Jim Albert}

\examples{
data <- c(2,4,3,6,1,0,4,3,10,2)
par <- c(0,1)
datapar <- list(data=data,par=par)
fit <- laplace(logpoissnormal, 1, datapar)
plot(fit, exact=TRUE, scale=4)
}
\keyword{models}
