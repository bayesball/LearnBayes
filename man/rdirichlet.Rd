\name{rdirichlet}
\alias{rdirichlet}
\title{Random draws from a Dirichlet distribution}
\description{
Simulates a sample from a Dirichlet distribution
}
\usage{
rdirichlet(n,par)
}
\arguments{
  \item{n}{number of simulations required}
  \item{par}{vector of parameters of the Dirichlet distribution}
}

\value{
matrix of simulated draws where each row corresponds to a single draw
}

\author{Jim Albert}

\examples{
par=c(2,5,4,10)
n=10
rdirichlet(n,par)
}

\keyword{models}
