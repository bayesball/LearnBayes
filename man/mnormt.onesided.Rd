\name{mnormt.onesided}
\alias{mnormt.onesided}
\title{Bayesian test of one-sided hypothesis about a normal mean}
\description{
Computes a Bayesian test of the hypothesis that a normal mean is 
less than or equal to a specified value}
\usage{
mnormt.onesided(m0,normpar,data)
}
\arguments{
  \item{m0}{value of the normal mean to be tested}
  \item{normpar}{vector of mean and standard deviation of the normal prior distribution}
  \item{data}{vector of sample mean, sample size, and known value of the population standard deviation}
}

\value{
\item{BF}{Bayes factor in support of the null hypothesis}
\item{prior.odds}{prior odds of the null hypothesis}
\item{post.odds}{posterior odds of the null hypothesis}
\item{postH}{posterior probability of the null hypothesis}
}

\author{Jim Albert}

\examples{
y=c(182,172,173,176,176,180,173,174,179,175)
pop.s=3
data=c(mean(y),length(data),pop.s)
m0=175
normpar=c(170,1000)
mnormt.onesided(m0,normpar,data)
}

\keyword{models}
