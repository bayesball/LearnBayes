\name{normpostsim}
\alias{normpostsim}
\title{Simulation from Bayesian normal sampling model}
\description{
 Gives a simulated sample from the joint posterior distribution of the mean and
variance for a normal sampling prior with a noninformative or
informative prior.  The prior assumes mu and sigma2 are independent with
mu assigned a normal prior with mean mu0 and variance tau2, and sigma2 is
assigned a inverse gamma prior with parameters a and b.
}
\usage{
normpostsim(data,prior=NULL,m=1000)
}
\arguments{
  \item{data}{vector of observations}
  \item{prior}{list with components mu, a vector with the prior mean 
  and variance, and sigma2, a vector of the inverse gamma parameters}
  \item{m}{number of simulations desired}
}

\value{
\item{mu}{vector of simulated draws of normal mean}
\item{sigma2}{vector of simulated draws of normal variance}
}
\author{Jim Albert}

\examples{
data(darwin)
s=normpostsim(darwin$difference)
}

\keyword{models}
