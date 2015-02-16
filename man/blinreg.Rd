\name{blinreg}
\alias{blinreg}
\title{Simulation from Bayesian linear regression model}
\description{
 Gives a simulated sample from the joint posterior distribution of the regression
vector and the error standard deviation for a linear regression model with a 
 noninformative or g prior.
}
\usage{
blinreg(y,X,m,prior=NULL)
}
\arguments{
  \item{y}{vector of responses}
  \item{X}{design matrix}
  \item{m}{number of simulations desired}
  \item{prior}{list with components c0 and beta0 of Zellner's g prior}
}

\value{
\item{beta}{matrix of simulated draws of beta where each row corresponds to one draw}
\item{sigma}{vector of simulated draws of the error standard deviation}
}
\author{Jim Albert}

\examples{
chirps=c(20,16.0,19.8,18.4,17.1,15.5,14.7,17.1,15.4,16.2,15,17.2,16,17,14.1)
temp=c(88.6,71.6,93.3,84.3,80.6,75.2,69.7,82,69.4,83.3,78.6,82.6,80.6,83.5,76.3)
X=cbind(1,chirps)
m=1000
s=blinreg(temp,X,m)
}

\keyword{models}
