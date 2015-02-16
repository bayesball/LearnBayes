\name{robustt}
\alias{robustt}
\title{Gibbs sampling for a robust regression model}
\description{
 Implements Gibbs sampling for a robust t sampling model with location mu, scale sigma, and degrees of freedom v }
\usage{
robustt(y,v,m)
}
\arguments{
 \item{y}{vector of data values}
  \item{v}{degrees of freedom for t model} 
  \item{m}{the number of cycles of the Gibbs sampler}
}
\value{
\item{mu}{vector of simulated values of mu}
\item{s2}{vector of simulated values of sigma2}
\item{lam}{matrix of simulated draws of lambda, where each row corresponds to a single draw}
}
\author{Jim Albert}

\examples{
data=c(-67,-48,6,8,14,16,23,24,28,29,41,49,67,60,75)
fit=robustt(data,4,1000)
}

\keyword{models}
