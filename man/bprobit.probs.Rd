\name{bprobit.probs}
\alias{bprobit.probs}
\title{Simulates fitted probabilities for a probit regression model}
\description{
 Gives a simulated sample for fitted probabilities for a binary response
regression model with a probit link and noninformative prior.
}
\usage{
bprobit.probs(X1,fit)
}
\arguments{
  \item{X1}{matrix where each row corresponds to a covariate set}
  \item{fit}{simulated matrix of draws of the regression vector}
}

\value{
matrix of simulated draws of the fitted probabilities, where a column corresponds to a
particular covariate set
}
\author{Jim Albert}

\examples{
response=c(0,1,0,0,0,1,1,1,1,1)
covariate=c(1,2,3,4,5,6,7,8,9,10)
X=cbind(1,covariate)
m=1000
fit=bayes.probit(response,X,m)
x1=c(1,3)
x2=c(1,8)
X1=rbind(x1,x2)
fittedprobs=bprobit.probs(X1,fit$beta)
}

\keyword{models}
