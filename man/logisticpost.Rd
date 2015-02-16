\name{logisticpost}
\alias{logisticpost}
\title{Log posterior for a binary response model with a logistic link and a uniform prior}
\description{
 Computes the log posterior density of (beta0, beta1)  when
yi are independent binomial(ni, pi) and logit(pi)=beta0+beta1*xi and
a uniform prior is placed on (beta0, beta1)
}
\usage{
logisticpost(beta,data)
}
\arguments{
  \item{beta}{vector of parameter values beta0 and beta1}
  \item{data}{matrix of columns of covariate values x, sample sizes n, and number of successes y}
}

\value{
value of the log posterior
}

\author{Jim Albert}

\examples{
x = c(-0.86,-0.3,-0.05,0.73)
n = c(5,5,5,5)
y = c(0,1,3,5)
data = cbind(x, n, y)
beta=c(2,10)
logisticpost(beta,data)
}

\keyword{models}
