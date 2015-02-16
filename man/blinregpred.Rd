\name{blinregpred}
\alias{blinregpred}
\title{Simulates values of predicted response for linear regression model}
\description{
Simulates draws of the predictive distribution of a future response for
a linear regression model with a noninformative prior}
\usage{
blinregpred(X1,theta.sample)
}
\arguments{
  \item{X1}{matrix where each row corresponds to a covariate set}
  \item{theta.sample}{list with components beta, matrix of simulated draws of regression vector,
and sigma, vector of simulated draws of sampling error standard deviation}
}

\value{
matrix where a column corresponds to the simulated draws of the predicted response for a given
covariate set
}

\author{Jim Albert}

\examples{
chirps=c(20,16.0,19.8,18.4,17.1,15.5,14.7,17.1,15.4,16.2,15,17.2,16,17,14.1)
temp=c(88.6,71.6,93.3,84.3,80.6,75.2,69.7,82,69.4,83.3,78.6,82.6,80.6,83.5,76.3)
X=cbind(1,chirps)
m=1000
theta.sample=blinreg(temp,X,m)
covset1=c(1,15)
covset2=c(1,20)
X1=rbind(covset1,covset2)
blinregpred(X1,theta.sample)
}

\keyword{models}
