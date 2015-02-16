\name{bayes.probit}
\alias{bayes.probit}
\title{Simulates from a probit binary response regression model using data augmentation and Gibbs sampling}
\description{
 Gives a simulated sample from the joint posterior distribution of the regression
vector for a binary response regression model with a probit link and a 
informative normal(beta, P) prior.  Also computes the log marginal likelihood when
a subjective prior is used.
}
\usage{
bayes.probit(y,X,m,prior=list(beta=0,P=0))
}
\arguments{
  \item{y}{vector of binary responses}
  \item{X}{covariate matrix}
  \item{m}{number of simulations desired}
  \item{prior}{list with components beta, the prior mean, and P, the prior precision matrix}
}

\value{
\item{beta}{matrix of simulated draws of regression vector beta where each row corresponds to one draw}
\item{log.marg}{simulation estimate at log marginal likelihood of the model}
}
\author{Jim Albert}

\examples{
response=c(0,1,0,0,0,1,1,1,1,1)
covariate=c(1,2,3,4,5,6,7,8,9,10)
X=cbind(1,covariate)
prior=list(beta=c(0,0),P=diag(c(.5,10)))
m=1000
s=bayes.probit(response,X,m,prior)
}

\keyword{models}
