\name{prior.two.parameters}
\alias{prior.two.parameters}
\title{Construct discrete uniform prior for two parameters}
\description{
Constructs a discrete uniform prior distribution for two parameters
}
\usage{
prior.two.parameters(parameter1, parameter2)
}
\arguments{
  \item{parameter1}{vector of values of first parameter}
  \item{parameter2}{vector of values of second parameter}
}
\value{
  matrix of uniform probabilities where the rows and columns are
labelled with the parameter values
}

\author{Jim Albert}

\examples{
prior.two.parameters(c(1,2,3,4),c(2,4,7))
}

\keyword{models}
